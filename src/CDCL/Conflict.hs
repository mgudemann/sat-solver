module CDCL.Conflict (analyzeConflict, calcReason) where

import           CDCL.Types (ActivityMap, Clause, ClauseList, Level (..),
                     Literal (..), MappedTupleList, Reason (..),
                     TupleClauseList, decreaseLvl, getLevel, getReason,
                     negateLiteralValue)

import           CDCL.Decisionalalgorithm (updateActivity)

import           CDCL.MapLogic (deleteLvl)

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set

-- | Complexity von O (n log n)
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

-- | Analyze Function. Calls the conflict analyse, updates MappedTupleList and adds the new learned clause
--  Example:
--  f = analyzeConflict  (Level 1) [Literal (-1), Literal (-2)] (Map.fromList [(Level 1, [((Literal (-1), BTrue), Decision), ((Literal 2, BTrue), Reason [Literal (-1), Literal 2])])]) [([Literal (-1), Literal 2], [Literal (-1), Literal 2]), ([Literal (-1), Literal (-2)], [Literal (-1), Literal (-2)])] Map.Empty
analyzeConflict :: Level -> Clause -> MappedTupleList -> ActivityMap -> (Level, Clause, MappedTupleList, ActivityMap)
analyzeConflict lvl emptyClause mtl aMap

    -- Case: Given Level is 0. Return -1
    | getLevel lvl == 0 = (Level (-1), [], mtl, aMap)
    | otherwise = (decreaseLvl lvl, fst newCl, updatedMtl, snd newCl)
    where reason = calcReason lvl emptyClause mtl
          updatedMtl = deleteLvl lvl mtl
          newCl = addClause reason aMap

-- | Calculate the reason of conflict. Uses calcReason' to calculate the 1UIP Clause and returns it.
--   E.G.
--   calcReason (Level 1) [Literal (-1), Literal 2] (Map.fromList [(Level 1, [ ((Literal (-1), BTrue), Decision), ((Literal 2, BFalse), Reason [Literal (-1), Literal (-2)]) ] )] )
--   calcReason (Level 1) [Literal 1] (Map.fromList [(Level 1, [((Literal 1, BFalse), Decision)])])
--   calcReason (Level 1) [Literal 1, Literal 2] (Map.fromList [(Level 1, [((Literal 1, BFalse), Decision), ((Literal 2, BTrue), Reason [Literal 2])])])
-- calcReason (Level 1) [Literal (-2), Literal (-3)] (Map.fromList [(Level 1, [((Literal 2, BTrue), Decision), ((Literal 3, BTrue), Reason [Literal (-2), Literal 3])])])
calcReason :: Level -> Clause -> MappedTupleList -> Clause
calcReason lvl emptyClause mtl

    -- Its not possible to enter this case, as analyzeConflict will return at Lvl 0
    | getLevel lvl == 0 = emptyClause
    | otherwise  = calc
    where associated = Map.lookup lvl mtl
          x = fromMaybe [] associated
          calc = calcReason' emptyClause x

-- | Calculate clauses until 1UIP-Clause is found. Return the found clause then.
calcReason' :: Clause -> TupleClauseList -> Clause
calcReason' cl tcl
    | length tcl > 1 = calcReason' unionCl reducedTcl
    | otherwise = cl
    where lastVal = last tcl
          reducedTcl = init tcl
          var = fst (fst lastVal)
          reason = getReason (snd lastVal)
          unionCl = unionClause cl reason var

-- | Add the newly calculated Clause to the ClauseList and
--   update the ActivityMap
addClause :: Clause -> ActivityMap  -> (Clause, ActivityMap)
addClause cl aMap
    | null cl = (cl, aMap)
    | otherwise = (updated, updatedAMap)
    where nubCl = rmdups cl
          updated = nubCl
          updatedAMap = updateActivity nubCl aMap

-- | Creates the new clause. Works by applying union to
--   empty clause with the reason. Also calls function to remove
--   the Literal which causes the conflict
unionClause :: Clause -> Clause -> Literal -> Clause
unionClause cl1 cl2 v = let list = cl1 `union` cl2 in
    removeLiteral list v

-- | remove the Literal which was the cause for the conflict from
--   the clause.
removeLiteral :: Clause -> Literal -> Clause
removeLiteral [] v = []
removeLiteral clause@(xs : ys) v
    | xs == v || xs == negateLiteralValue v = removeLiteral ys v
    | otherwise = xs : removeLiteral ys v

