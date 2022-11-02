---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Unitpropagation
-- Description :   Contains necessary logic for unitpropagation
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Unitpropagation (getUnitClause, unitSubsumption,
    unitResolution, unitPropagation, unitSubsumeResolve) where

import           CDCL.Types (BoolVal (..), Clause, ClauseList, Level,
                     Literal (..), MappedTupleList, Reason (..),
                     ReducedClauseAndOGClause, TriTuple, Tuple,
                     TupleClauseList, getClauseFromReducedClauseAndOGClause,
                     getLiteralValue, getOGFromReducedClauseAndOGClause,
                     negateLiteralValue)

import           CDCL.MapLogic (pushToMappedTupleList)

import qualified Data.Set as Set (delete, elemAt, empty, member)

-- | The function is the base for the unitpropagation procedure. It checks first if an
--   unitclause exists. If it does, it will set the Literal so that the unitclause is solved.
unitPropagation :: ClauseList -> TupleClauseList -> Level -> MappedTupleList -> TriTuple
unitPropagation clist tlist lvl mapped

    -- Case: no UnitClause found or no more clauses in ClauseList
    | null clist || null fstElem = (clist, tlist, mapped)
    | otherwise = unitPropagation resolutionC (tlist ++ [(calcTuple, ogClause)]) lvl updatedMap
    where
          unitClause = getUnitClause clist
          fstElem = getClauseFromReducedClauseAndOGClause unitClause
          calcTuple = setLiteral fstElem
          ogClause = Reason (getOGFromReducedClauseAndOGClause unitClause)
          updatedMap = pushToMappedTupleList mapped lvl calcTuple ogClause
          resolutionC = unitSubsumeResolve clist calcTuple []

-- | checks if an unit clause exists in the given list of lists. if one exists return the list.
getUnitClause :: ClauseList  -> ReducedClauseAndOGClause
getUnitClause (clause : xs) = let listLength = length (getClauseFromReducedClauseAndOGClause clause) in
    if listLength == 1 then clause else getUnitClause xs

getUnitClause _ = (Set.empty, Set.empty)

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setLiteral :: Clause  -> Tuple
setLiteral clause =
  let e = Set.elemAt 0 clause
  in  if getLiteralValue e < 0
      then (negateLiteralValue e, BFalse) else (e, BTrue) -- Need change here

-- | Remove clauses which have removableVar as Literal.
unitSubsumption :: ClauseList  -> Tuple -> ClauseList -> ClauseList
unitSubsumption (firstList@(rClause, _) : xs) tuple@(Lit x, bVal) acc

    -- Case: Literal is not found in current clause. Readd it to the ClauseList
    | not checked = unitSubsumption xs tuple (firstList : acc)

    -- Case: Literal was found. Remove the clause.
    | otherwise = unitSubsumption xs tuple acc
    where val = if bVal == BTrue then Lit x else Lit (-x)
          checked = val `Set.member` rClause -- checks if val is inside list

unitSubsumption _ _ acc = acc

-- | remove negated Literal of the Literal which was set
--   For example a negated Literal was resolved, which would remove
--   the positive ones.
unitResolution :: ClauseList -> Tuple -> ClauseList -> ClauseList
unitResolution ((rClause, ogClause) : xs) tuple@(Lit x, bVal) acc =
  unitResolution xs tuple ((list, ogClause) : acc)
    where val = if bVal == BFalse then Lit x else Lit (-x)
          list = Set.delete val rClause

unitResolution _ _ acc = acc


unitSubsumeResolve :: ClauseList -> Tuple -> ClauseList -> ClauseList
unitSubsumeResolve [] _ acc = acc
unitSubsumeResolve ((rClause, ogClause) : xs) tuple@(Lit x, bVal) acc =
  let valSubsume = if bVal == BTrue  then Lit x else Lit (-x)
      valResolve = if bVal == BFalse then Lit x else Lit (-x)
      list       = Set.delete valResolve rClause
  in  if valSubsume `Set.member` rClause
      then unitSubsumeResolve xs tuple acc
      else unitSubsumeResolve xs tuple ((list, ogClause) : acc)
