{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CDCL.DPLL (rmPureVars) where

import Data.Set

rmPureVars :: [[Integer]] -> ([[Integer]], [Int])
rmPureVars term = rmPureVars' (term, [])

rmPureVars' :: ([[Integer]], [Int]) -> ([[Integer]], [Int])
rmPureVars' t = if t == t' then t else rmPureVars' t'
    where t' = rmPureVars'' t

rmPureVars'' :: ([[Integer]], [Int]) -> ([[Integer]], [Int])
rmPureVars'' (term, removedVars) = (reduzedTerm, removedVars ++ pureVars)
    where pureVars = getPureVars term
          reduzedTerm = rmVars term pureVars

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

isPureVar :: [[Integer]] -> Integer -> Bool
isPureVar [] _ = True
isPureVar (x:xs) var = (-var) `notElem` x && isPureVar xs var

rmVars :: [[Integer]] -> [Int] -> [[Integer]]
rmVars = Prelude.foldl rmVar

rmVar :: [[Integer]] -> Int -> [[Integer]]
rmVar [] _ = []
rmVar (x:xs) var = [x | i `notElem` x] ++ rmVar xs var
    where i = toInteger var
