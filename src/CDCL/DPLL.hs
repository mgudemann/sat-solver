{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CDCL.DPLL (rmPureVars) where

import Data.Set

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
    where pureVars = getPureVars term
          reduzedTerm = rmVars term pureVars

-- | This function finds all pure varibales in a given term.
--   It a list of pure variables
getPureVars :: [[Integer]] -> [Int]
getPureVars term = result
    where pureVars = getPureVars' term term -- get pureVars
          set = fromList pureVars --remove duplicates
          result = toList set

getPureVars' :: [[Integer]] -> [[Integer]] -> [Int]
getPureVars' _ [] = []
getPureVars' fullerm (x:xs) = getPureVars'' fullerm x ++ getPureVars' fullerm xs

getPureVars'' :: [[Integer]] -> [Integer] -> [Int]
getPureVars'' _ [] = []
getPureVars'' fullerm (x:xs) = [i | isPureVar fullerm x] ++ getPureVars'' fullerm xs
    where i = fromIntegral x

-- | This function decides if a given variable is pure in the given term.
isPureVar :: [[Integer]] -> Integer -> Bool
isPureVar [] _ = True
isPureVar (x:xs) var = (-var) `notElem` x && isPureVar xs var

-- | This function finds removesall all clauses that contains the given pure variables.
rmVars :: [[Integer]] -> [Int] -> [[Integer]]
rmVars = Prelude.foldl rmVar

-- | This function finds removesall all clauses that contains the given pure variable.
rmVar :: [[Integer]] -> Int -> [[Integer]]
rmVar [] _ = []
rmVar (x:xs) var = [x | i `notElem` x] ++ rmVar xs var
    where i = toInteger var
