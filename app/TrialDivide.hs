module TrialDivide (trialDivide) where

import Primes

trialDivide :: Integral a => a -> [(a,Int)]
trialDivide n = collectDuplicates 0 0 $ trialDivide' n primes

trialDivide' :: Integral a => a -> [a] -> [a]
trialDivide' n pp@(p:ps)
  | n == 1 = []
  | n < p * p = [n]
  | n `mod` p == 0 = p : trialDivide' (n `div` p) pp
  | otherwise = trialDivide' n ps
trialDivide' _ [] = undefined

collectDuplicates :: Eq a => a -> Int -> [a] -> [(a,Int)]
collectDuplicates p e [] = [(p,e)]
collectDuplicates p e (n:ns)
  | p == n = collectDuplicates p (e + 1) ns
  | e == 0 = collectDuplicates n 1 ns
  | otherwise = (p,e) : collectDuplicates n 1 ns
