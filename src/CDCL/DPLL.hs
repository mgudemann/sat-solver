{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CDCL.DPLL (rmPureVars) where

import           Data.Set (Set, (\\))
import qualified Data.Set as Set

-- | This function removes all clauses that contains pure variables
--   and thus that become pure due to reductions.
--   It retruns te reduced term and a list of pure variables
rmPureVars
    :: [[Integer]]          -- ^ The '[[Integer]]' argument repesents the input term.
    -> ([[Integer]], [Int]) -- ^ The '([[Integer]], [Int])' result repesents the reduced term as the first element and the pure vars as the second element.
rmPureVars term = rmPureVars' (term, [])

rmPureVars' :: ([[Integer]], [Int]) -> ([[Integer]], [Int])
rmPureVars' t = if t == t' then t else rmPureVars' t'
    where t' = rmPureVars'' t

rmPureVars'' :: ([[Integer]], [Int]) -> ([[Integer]], [Int])
rmPureVars'' (term, removedVars) = (reduzedTerm, removedVars ++ pureVars)
    where pureVars = collectPure term -- getPureVars term
          reduzedTerm = rmVars term pureVars

collectPure :: [[Integer]] -> [Int]
collectPure cls =
  let pos = fmap (Set.fromList . map fromIntegral . Prelude.filter (> 0)) cls
      neg = fmap (Set.fromList . map (fromIntegral . (\x -> -x)) . Prelude.filter (< 0)) cls
      negSet' = Set.unions neg
      posSet' = Set.unions pos
      negSet  = negSet' \\ posSet'
      posSet  = posSet' \\ negSet'
  in  Set.toList (Set.map (*(-1)) negSet `Set.union` posSet)

-- | This function finds removesall all clauses that contains the given pure variables.
rmVars :: [[Integer]] -> [Int] -> [[Integer]]
rmVars = Prelude.foldl rmVar

-- | This function finds removesall all clauses that contains the given pure variable.
rmVar :: [[Integer]] -> Int -> [[Integer]]
rmVar [] _ = []
rmVar (x:xs) var = [x | i `notElem` x] ++ rmVar xs var
    where i = toInteger var
