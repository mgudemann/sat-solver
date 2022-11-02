{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CDCL.DPLL (rmPureVars) where

import           Data.Set ((\\))
import qualified Data.Set as Set

-- | This function removes all clauses that contains pure variables
--   and thus that become pure due to reductions.
--   It retruns te reduced term and a list of pure variables
rmPureVars
    :: [[Int]]          -- ^ The '[[Int]]' argument repesents the input term.
    -> ([[Int]], [Int]) -- ^ The '([[Int]], [Int])' result repesents the reduced term as the first element and the pure vars as the second element.
rmPureVars term = rmPureVars' (term, [])

rmPureVars' :: ([[Int]], [Int]) -> ([[Int]], [Int])
rmPureVars' t = if t == t' then t else rmPureVars' t'
    where t' = rmPureVars'' t

rmPureVars'' :: ([[Int]], [Int]) -> ([[Int]], [Int])
rmPureVars'' (term, removedVars) = (reduzedTerm, removedVars ++ pureVars)
    where pureVars = collectPure term -- getPureVars term
          reduzedTerm = rmVars term pureVars

collectPure :: [[Int]] -> [Int]
collectPure cls =
  let pos = fmap (Set.fromList . Prelude.filter (> 0)) cls
      neg = fmap (Set.fromList . map (\x -> -x) . Prelude.filter (< 0)) cls
      negSet' = Set.unions neg
      posSet' = Set.unions pos
      negSet  = negSet' \\ posSet'
      posSet  = posSet' \\ negSet'
  in  Set.toList (Set.map (*(-1)) negSet `Set.union` posSet)

-- | This function finds removesall all clauses that contains the given pure variables.
rmVars :: [[Int]] -> [Int] -> [[Int]]
rmVars = Prelude.foldl rmVar

-- | This function finds removesall all clauses that contains the given pure variable.
rmVar :: [[Int]] -> Int -> [[Int]]
rmVar [] _ = []
rmVar (x:xs) var = [x | i `notElem` x] ++ rmVar xs var
    where i = var
