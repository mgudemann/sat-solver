{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
----------------------------------------------------------------------- |
-- Module      :   CDCL.Algorithm
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
-- = Description
-- General algorithms for DPLL and CDCL Algorithm.
-- Includes interpreting clauses, clauseLists,
-- tuple values and calculating ClauseList.
---------------------------------------------------------------------


module CDCL.Algorithm (cdcl, solve, interpret, searchTuple) where

import           CDCL.Decisionalalgorithm (getHighestActivity,
                     getShortestClauseViaActivity, halveActivityMap,
                     initialActivity, setLiteralViaActivity)

import           CDCL.Types (Activity (..), ActivityMap, BoolVal (..),
                     CDCLResult (..), Clause, ClauseList, InterpretResult (..),
                     Level (..), Literal (..), MappedTupleList, Period (..),
                     Reason (..), SATResult (..), TriTuple, Tuple,
                     TupleClauseList, decreasePeriod,
                     getClauseFromReducedClauseAndOGClause, getEmptyClause,
                     getLiteralValue, getNOK,
                     getOGFromReducedClauseAndOGClause, increaseLvl,
                     negateLiteralValue, transformClauseList)

import           CDCL.Unitpropagation (unitPropagation, unitResolution,
                     unitSubsumption)

import           CDCL.MapLogic (pushToMappedTupleList)

import           CDCL.Conflict (analyzeConflict)
import           CDCL.DPLL (rmPureVars)

import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set (deleteAt, elemAt)


hardCoded = Period 30
startBoundary = 20


decodeTuple :: Int -> Tuple
decodeTuple value = if value >= 0
    then (Lit (toInteger value), BTrue)
    else (Lit (toInteger (-value)), BFalse)


encodeTuple :: Tuple -> Int
encodeTuple (Lit l, BTrue) = fromInteger l
encodeTuple (Lit l, BFalse) = fromInteger (-l)

-- | This function calls the solver without reporting any statistics. Returns
-- list of literanls encoded as Integer.
solve :: [[Integer]] -> SATResult
solve t = case result of
            SAT                               -> error "solution must be found for SAT"
            SAT_SOLUTION      l               -> Satisfiable (map encodeTuple l)
            SAT_WITH_STATS l _ _ _ _          -> Satisfiable (map encodeTuple l)
            SAT_WITH_FULL_STATS l _ _ _ _ _ _ -> Satisfiable (map encodeTuple l)
            UNSAT                             -> Unsatisfiable
            UNSAT_WITH_STATS _ _              -> Unsatisfiable
            where result = cdcl t True False False

-- | This function will start the CDCL Procedure.
--   To call this function do for example:
--   cdcl [[1,2,3],[2,5]]
--   cdcl [[1,2,3,4], [2,4], [4,5],[3,6,7],[3,9,1],[3,8,10]]
--   The function will return the result of the cdcl' function.
--   Function will immediately return SAT if ClauseList is null or UNSAT if an empty List is found within clist
cdcl :: [[Integer]] -> Bool -> Bool -> Bool -> CDCLResult
cdcl clist valuation stats fullStats
    | checked = UNSAT
    | null clist && fullStats = SAT_WITH_FULL_STATS [] Map.empty [] 0 0 0 0
    | null clist && stats = SAT_WITH_STATS [] 0 0 0 0
    | null clist = SAT --[] Map.empty 0
    | otherwise = case (valuation, result) of
                    (False, SAT_SOLUTION _)                                                                                 -> SAT
                    (False, SAT_WITH_STATS _ _ _ _ _)                                                                       -> SAT
                    (False, SAT_WITH_FULL_STATS _ _ _ _ _ _ _)                                                              -> SAT
                    (True, SAT_SOLUTION tupleList)                                                                          -> SAT_SOLUTION (tupleList ++ (map decodeTuple pureVars))
                    (True, SAT_WITH_STATS tupleList decisions learned clauses restarts)                                     -> SAT_WITH_STATS (tupleList ++ (map decodeTuple pureVars)) decisions learned clauses restarts
                    (True, SAT_WITH_FULL_STATS tupleList mappedTupleList learnedClauses decisions learned clauses restarts) -> SAT_WITH_FULL_STATS (tupleList ++ (map decodeTuple pureVars)) mappedTupleList learnedClauses decisions learned clauses restarts
                    _                                                                                                       -> result
    where (reducedTerm, pureVars) = rmPureVars clist
          checked = any null reducedTerm
          transformedList = transformClauseList reducedTerm
          aMap = initialActivity transformedList Map.empty
          result = cdcl'
                  aMap
                  (Level 0)
                  []
                  Map.empty
                  transformedList
                  transformedList
                  []
                  []
                  transformedList
                  hardCoded
                  0
                  (startBoundary * 2)
                  startBoundary
                  stats
                  fullStats
                  0


-- | Function will first call the Unitpropagation Function.
--   Afterwards it will check if every Clause is interpreted.
--   If that isn't the case it will call functions related to
--   the Decision Algorithm.
--   In the case of getting NOK as result the function will call
--   the analyzeConflict Function to resolve the conflict.
--   After that the recursion starts again with Unitpropagation.
--   This happens until either SAT or UNSAT is returned as result.
cdcl'
  :: ActivityMap
  -> Level
  -> TupleClauseList
  -> MappedTupleList
  -> ClauseList
  -> ClauseList
  -> [Clause]
  -> [Clause]
  -> ClauseList
  -> Period
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> Bool
  -> Integer
  -> CDCLResult
cdcl' aMap (Level lvl)  tlist mappedTL clistOG learnedClist learnedClauses confClauses clist period conflictIteration upperBound currentBoundary stats fullStats restarts

    -- First and Second Case are part of Restart Algorithm with Luby Sequence
    -- current conflictiteration has same value like the current upper boundary. Restart the algorithm with higher upper boundary
    | conflictIteration == upperBound = cdcl' (initialActivity clistOG Map.empty)
                                              (Level 0)
                                              []
                                              Map.empty
                                              clistOG
                                              learnedClist
                                              learnedClauses
                                              confClauses
                                              learnedClist
                                              hardCoded
                                              0
                                              (upperBound*2)
                                              startBoundary
                                              stats
                                              fullStats
                                              (restarts + 1)

    -- current conflictiteration has same value like the current restart boundary. Restarts the algorithm with higher current boundary
    | conflictIteration == currentBoundary = cdcl' (initialActivity clistOG Map.empty)
                                                   (Level 0)
                                                   []
                                                   Map.empty
                                                   clistOG
                                                   learnedClist
                                                   learnedClauses
                                                   confClauses
                                                   learnedClist
                                                   hardCoded
                                                   0
                                                   upperBound
                                                   (currentBoundary * 2)
                                                   stats
                                                   fullStats
                                                   (restarts + 1)

    -- Interpret returned a NOK. Start the conflict analysis.
    | getNOK interpreted =
        let empty    = getEmptyClause interpreted
            analyzed = analyzeConflict (Level lvl) empty updatedMap halvedActivity
        in
          if getLevelFromAnalyze analyzed == Level (-1) then do
              if stats || fullStats then  UNSAT_WITH_STATS (reverse (getClauseFromAnalyze analyzed :learnedClauses)) (reverse (empty :confClauses)) else UNSAT
                else cdcl' (getActivityMapFromAnalyze analyzed)
                           (getLevelFromAnalyze analyzed)
                           (makeTupleClauseListFromAnalyze analyzed)
                           (getMappedTupleListFromAnalyze analyzed)
                           clistOG
                           ((getClauseFromAnalyze analyzed, getClauseFromAnalyze analyzed) : learnedClist) -- learnedClist
                           (getClauseFromAnalyze analyzed : learnedClauses) -- learnedClauses
                           (empty : confClauses)
                           (calculateClauseList ((getClauseFromAnalyze analyzed, getClauseFromAnalyze analyzed) : learnedClist)
                           (makeTupleClauseListFromAnalyze analyzed))
                           periodUpdate2
                           (conflictIteration + 1)
                           upperBound
                           currentBoundary
                           stats
                           fullStats
                           restarts

    -- Interpret retunred OK. Stop the algorithm.
    -- Returns everx statistics
    | interpreted == OK && fullStats = SAT_WITH_FULL_STATS (map fst tupleRes) updatedMap learnedClauses
        (getDecisions updatedMap 0 0) (toInteger(length (Map.keys updatedMap))) (toInteger (length learnedClist - length clistOG)) restarts
    -- Returns some statistics
    | interpreted == OK && stats = SAT_WITH_STATS (map fst tupleRes) (getDecisions updatedMap 0 0) (toInteger(length (Map.keys updatedMap))) (toInteger (length learnedClauses)) restarts
    -- Returns no statistics
    | interpreted == OK = SAT_SOLUTION (map fst tupleRes)
    | otherwise = cdcl' halvedActivity
                        newLvl
                        list
                        updateMapViaDecision
                        clistOG
                        learnedClist
                        learnedClauses
                        confClauses
                        (calculateClauseList (getClauseListFromTriTuple res) list)
                        periodUpdate2
                        conflictIteration
                        upperBound
                        currentBoundary
                        stats
                        fullStats
                        restarts
    where res = unitPropagation clist tlist (Level lvl) mappedTL
          tupleRes = getTupleClauseListFromTriTuple res
          updatedMap = getMappedTupleListFromTriTuple res
          interpreted = interpret learnedClist tupleRes

          periodUpdate = decreasePeriod period
          halvedActivity = if periodUpdate == Period 0 then halveActivityMap aMap (Map.keys aMap) else aMap
          periodUpdate2 = if periodUpdate == Period 0 then hardCoded else periodUpdate

          newLvl = increaseLvl (Level lvl)
          highestActivity = getHighestActivity (getClauseListFromTriTuple res) aMap [(Lit 0, Activity (-1))]
          shortestCl = getShortestClauseViaActivity (getClauseListFromTriTuple res) [] highestActivity
          firstShortestCl = head shortestCl

          assuredShortestClause = getClauseFromReducedClauseAndOGClause firstShortestCl
          firstHighestActivityInClause = getHighestActivity [firstShortestCl] aMap [(Lit 0, Activity (-1))]
          decided = setLiteralViaActivity assuredShortestClause (head firstHighestActivityInClause) -- Need change here? it takes first highest found LiteralActivity in the clause.
          updateMapViaDecision = uncurry (pushToMappedTupleList updatedMap newLvl) decided
          list = decided : tupleRes

-- | calculates the clauselist which will be given to unitpropagation.
--   returns when everything of tupelClauselist was calculated
calculateClauseList :: ClauseList -> TupleClauseList -> ClauseList
calculateClauseList cl tlist@(xs : ys)
    | null ys = reso
    | otherwise = calculateClauseList reso ys
    where sub = unitSubsumption cl (fst xs) []
          reso = unitResolution sub (fst xs) []
calculateClauseList cl [] = cl

-- | Interprets a given ClauseList based on a given TupleClauseList. Will call itself recursively
--   until a UNRESOLVED, NOK or OK is returned
--   Bsp: [[2,1,3],[-1]] [(1,0),(3,0),(2,0)] -> 0. CONFLICT
--   Bsp: [[2,1,3]][(1,0),(2,0)] -> -1. Etwas wurde noch nicht belegt o. etwas wurde nicht positiv.
interpret :: ClauseList -> TupleClauseList -> InterpretResult
interpret [] _ = OK
interpret t@(formel : xs) interpretation

    -- Case: Clause found which cant be evaluated to 0 or 1
    | null interpretation || interpreted == UNRESOLVED = UNRESOLVED

    -- Case: Clause found which evalutes to 0.
    | getNOK interpreted = NOK (getOGFromReducedClauseAndOGClause formel)

    -- Case: None of the above cases appeared. Continue evaluating clauseList
    | not (null xs) = interpret xs interpretation

    -- Case: Returns OK as Result
    | otherwise = interpreted --interpret' (snd formel) interpretation False
    where interpreted = interpret' (getOGFromReducedClauseAndOGClause formel) interpretation False

-- | Interprets a single clause of a formula
--   It will return either
--   OK
--   NOK (emptyClause) <-- Clause which returns 0 with the set Literals.
--   UNRESOLVED <-- No Literal evaluated the clause to 1.
interpret' :: Clause -> TupleClauseList -> Bool -> InterpretResult
interpret' clause interpretation boolValue

    -- if calculated tupelValue isn't set and xs is null return UNRESOLVED
    | tupelValue == BNothing   && null xs = UNRESOLVED

    -- tupelValue not set but xs isn't null. Continue with the iteration.
    | tupelValue == BNothing  && not (null xs) = interpret' xs interpretation True

    -- The given Literal evalutes to 1. Returns OK
    | (formelValue >= 0 && tupelValue == BTrue) || (formelValue < 0 && tupelValue == BFalse) = OK

    -- Current Literal evalutes to 0 and xs is null. If it never entered to the second case it will return NOK. Otherwise it will enter below case to return UNRESOLVED
    | ((formelValue >= 0 && tupelValue == BFalse) || (formelValue < 0 && tupelValue == BTrue))  && null xs && not boolValue = NOK  clause

    | boolValue && null xs = UNRESOLVED
    | otherwise = interpret' xs interpretation boolValue
        where formel      = Set.elemAt 0 clause
              xs          = Set.deleteAt 0 clause
              formelValue = getLiteralValue formel
              varValue = if formelValue < 0 then negateLiteralValue formel else formel
              tupelValue = searchTuple varValue interpretation

-- | Get the set value from the tupelClauselist.
searchTuple :: Literal -> TupleClauseList -> BoolVal
searchTuple xval (xs : ys)
    | fst tuple == xval = snd tuple
    | not (null ys) = searchTuple xval ys
    | otherwise = BNothing
    where tuple = fst xs

searchTuple _ [] = BNothing

-- | returns the clauseList from unitPropagation
getClauseListFromTriTuple :: TriTuple -> ClauseList
getClauseListFromTriTuple (x, _, _) = x

-- | returns the TupleClauseList from unitPropagation
getTupleClauseListFromTriTuple ::  TriTuple -> TupleClauseList
getTupleClauseListFromTriTuple (_, x, _) = x

-- | returns the MappedTupleList from unitPropagation
getMappedTupleListFromTriTuple :: TriTuple -> MappedTupleList
getMappedTupleListFromTriTuple (_, _, x) = x

-- | returns the new level after analyzing the conflict
getLevelFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> Level
getLevelFromAnalyze (x, _, _, _) = x

-- | returns the new clause after analyzing the conflict
getClauseFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> Clause
getClauseFromAnalyze (_, x, _, _) = x

-- | returns the new mappedtupleList after analyzing the conflict
getMappedTupleListFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> MappedTupleList
getMappedTupleListFromAnalyze (_, _, x, _) = x

-- | function returns a tupleclauseList based on MappedTupleList from analyzing the conflict
makeTupleClauseListFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> TupleClauseList
makeTupleClauseListFromAnalyze  = concat . getMappedTupleListFromAnalyze

-- | returns the new activityMap after analyzing the conflict
getActivityMapFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> ActivityMap
getActivityMapFromAnalyze (_, _, _, x) = x

-- | Returns the amount of decisions taken.
getDecisions :: MappedTupleList -> Integer -> Integer -> Integer
getDecisions mtl int found
    | null arr && int == 0 = getDecisions mtl (int + 1) found
    | null arr = found
    | snd (head arr) == Decision = getDecisions mtl (int + 1) (found + 1)
    | snd (head arr) /= Decision && int == 0 = getDecisions mtl (int + 1) 0
    where arr = fromMaybe [] (Map.lookup (Level int) mtl)
