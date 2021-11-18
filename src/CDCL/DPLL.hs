{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CDCL.DPLL (rmPureVars, getPureVars, isPureVar, rmVars) where

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
isPureVar (x:xs) var = (not (containsVar x (-var))) && isPureVar xs var

containsVar :: [Integer] -> Integer -> Bool
containsVar [] _ = False
containsVar (x:xs) var = (x == var) || containsVar xs var

rmVars :: [[Integer]] -> [Int] -> [[Integer]]
rmVars term [] = term
rmVars term (x:xs) = rmVars (rmVar term x ) xs

rmVar :: [[Integer]] -> Int -> [[Integer]]
rmVar [] _ = []
rmVar (x:xs) var = [x | not (containsVar x i)] ++ rmVar xs var
    where i = toInteger var
