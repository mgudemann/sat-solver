{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Decisionalgorithm
-- Description :   Contains logic related to the decision algorithm
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Decisionalalgorithm (initialActivity, updateActivity, halveActivityMap,
    getHighestActivity, setLiteralViaActivity, getShortestClauseViaActivity) where

import           CDCL.Types (Activity (..), ActivityMap, BoolVal (..), Clause,
                     ClauseList, Literal (..), LiteralActivity, Reason (..),
                     Tuple, TupleClause, divideActivity, getActivityValue,
                     getClauseFromReducedClauseAndOGClause, getLiteralValue,
                     increaseActivity, negateLiteralValue)
import qualified CDCL.Types as TypeC
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set (deleteAt, elemAt, null)

-- | calculate the ActivityMap. calls itself recursively until every clause
--   is calculated. Returns a filled ActivityMap.
--   example: initialActivity [[1,2,3,4],[3,4]] (IntMap.fromList [])
--   result: fromList [(1,1),(2,1),(3,2),(4,2)]
initialActivity :: ClauseList -> ActivityMap -> ActivityMap
initialActivity cList@(xs : ys) aList
    | not (null ys) = initialActivity ys updated
    | otherwise = updated
    where updated = updateActivity (getClauseFromReducedClauseAndOGClause xs) aList
-- initialActivity x aList = updateActivity (fst (head x)) aList

-- | updates the activitymap.
--   example : updateActivity [1,2] (IntMap.fromList [(1,1),(2,1),(3,2),(4,2)])
--   result : fromList [(1,2),(2,2),(3,2),(4,2)]
updateActivity :: Clause -> ActivityMap -> ActivityMap
updateActivity clause aMap
 | Set.null clause = aMap
-- updateActivity clause@(xs : ys) aMap

    -- Current Literal is already in Map.
 | Map.member xValue aMap = let updatedMap = Map.adjust increaseActivity xValue aMap in
                              updateActivity ys updatedMap

    -- Current Literal is not in the Map.
 |  Map.notMember xValue aMap = let updatedMap = Map.insert xValue (Activity 1) aMap in
                                  updateActivity ys updatedMap
    where xs = Set.elemAt 0 clause
          ys = Set.deleteAt 0 clause
          xValue = if getLiteralValue xs < 0 then negateLiteralValue xs else xs


-- | periodically call this function to half the activities in the map.
halveActivityMap :: ActivityMap -> [Literal] -> ActivityMap
halveActivityMap = foldr (Map.adjust divideActivity)
-- above code does following thing:
-- halveActivityMap aMap (xs : ys) = let updateMap = Map.adjust divideActivity xs aMap
--     in halveActivityMap updateMap ys
-- halveActivityMap aMap [] = aMap

-- | Return the highest Activity which can be found in the ClauseList. Calls itself recursively
--   until every clauses was calculated.
--   example : getHighestActivity [([Literal (-1), Literal 3, Literal 5],[Literal (-1), Literal 3, Literal 5]) ,
--   ([Literal 3, Literal 7],[Literal 3, Literal 7])]
--   (Map.fromList [(Literal 1, Activity 5),(Literal 3, Activity 6),(Literal 5,Activity 2),(Literal 7,Activity 7)]) (Literal 0, Activity 0)
getHighestActivity :: ClauseList -> ActivityMap -> [LiteralActivity] -> [LiteralActivity]
getHighestActivity cList@(xs : ys) aMap val

    -- Case: Found Activity is higher then current activity
    | getActivityValue (snd firstVal) < getActivityValue (snd foundAct) = getHighestActivity ys aMap highestValInClause

    -- Case: Found Activity is lower then current activity
    | getActivityValue (snd firstVal) > getActivityValue (snd foundAct) = getHighestActivity ys aMap val

    -- Case: Found activity has the same value
    | getActivityValue (snd firstVal) == getActivityValue (snd foundAct) = getHighestActivity ys aMap list
    where firstVal = head val
          highestValInClause = getHighestActivity' (getClauseFromReducedClauseAndOGClause xs) aMap val
          firstActVal = getActivityValue (snd firstVal)
          foundAct = head highestValInClause
          list = nub (val ++ highestValInClause)

getHighestActivity [] aMap val = val

-- | return the highest activity in a clause.
--   example getHighestActivity' [-1,3,5] (Map.fromList [(1,5),(3,6),(5,2)]) (0,0)
--   getHighestActivity' [-1,2] (Map.fromList [(1,1),(2,1)]) (0,0)
--   returns (3,6)
getHighestActivity' :: Clause -> ActivityMap -> [LiteralActivity] -> [LiteralActivity]
getHighestActivity' cl aMap val
 | Set.null cl = val

 -- Case: found activity is higher then current activity
 | actVal > snd firstVal = getHighestActivity' ys aMap [(x, actVal)]

 -- Case: found activity has same activity value
 | actVal == snd firstVal = getHighestActivity' ys aMap ((x, actVal) : val)

 -- Case: found activity has lower activity value
 | otherwise = getHighestActivity' ys aMap val
    where xs = Set.elemAt 0 cl
          ys = Set.deleteAt 0 cl
          firstVal = head val
          x = if getLiteralValue xs < 0 then negateLiteralValue xs else xs
          actVal = Map.findWithDefault (Activity 0) x aMap

-- | Set the Tupelvalue based on the Literal.
--   If the Literal with the highest activity has a minus prefix the tupel value will
--   be set to 1 with the Literal getting a positive prefix in the tupel.
--   Else the tupel will be set to the Literal with a 0 as second value.
setLiteralViaActivity :: Clause -> LiteralActivity -> TupleClause
setLiteralViaActivity clause vAct
     --((Literal (-1), BNothing), Reason [Literal (-1)])
 | Set.null clause = error "wrong input in LiteralActivity or Clause"


 -- Case: the current Literal is a positive one and is found in vAct.
 | xs == fst vAct = ((xs, BFalse), Decision)

 -- Case: the current Literal is a negative one and is found in vAct when negated.
 | negateLiteralValue xs == fst vAct = ((negateLiteralValue xs, BTrue), Decision)
 | otherwise = setLiteralViaActivity ys vAct
  where xs = Set.elemAt 0 clause
        ys = Set.deleteAt 0 clause


-- | Get the shortest clause which contains the highest activity.
--   Do this based on the given ClauseList and LiteralActivity. Returns
--   a ClauseList
getShortestClauseViaActivity :: ClauseList -> ClauseList -> [LiteralActivity] -> ClauseList
getShortestClauseViaActivity (xs : ys) checkC vAct

    -- Case: checkC is null and checkClause found Clause which contains a LiteralActivity inside vAct
    | null checkC && checkClause = getShortestClauseViaActivity ys [xs] vAct

    -- Case: either checkClause doesnt find LiteralActivty in clause or the length of current clause is bigger then current shortest Clause
    | not checkClause || xsLen > headLen = getShortestClauseViaActivity ys checkC vAct

    -- Case: Current Clause is shorter then the ones in checkC. Also no other clause has the same length
    | null filterClause && xsLen < headLen  = [xs]

    -- Case: Current Clause has the same length like current shortest one
    | xsLen == headLen = getShortestClauseViaActivity ys  (xs : checkC) vAct

    -- Case: See 3rd Case but ther are still other clauses which are either the same length or shorter.
    | xsLen < headLen = getShortestClauseViaActivity filterClause [xs] vAct
    where checkClause = checkClauseForLiteral (getClauseFromReducedClauseAndOGClause xs) vAct
          xsLen = length (getClauseFromReducedClauseAndOGClause xs)
          headLen = length (getClauseFromReducedClauseAndOGClause (head checkC))
          filterClause = filter (\x -> length (getClauseFromReducedClauseAndOGClause  x) <= length (getClauseFromReducedClauseAndOGClause xs)) ys

    --  firstVal `elem` fst xs || negateLiteralValue firstVal `elem` fst xs = Just (fst xs)
    --  otherwise = getShortestClauseViaActivity ys vAct
    --where firstVal = fst vAct
getShortestClauseViaActivity [] checkC _ = checkC

-- | Checks if the given Clause contains a given LiteralActivity. Returns true if it does
--   else return false.
checkClauseForLiteral :: Clause -> [LiteralActivity] -> Bool
checkClauseForLiteral cl (x : ys)
    | fst x `elem` cl || negateLiteralValue (fst x) `elem` cl = True
    | otherwise = checkClauseForLiteral cl ys
checkClauseForLiteral cl [] = False
