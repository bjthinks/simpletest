module Main where

import Primes
import TrialDivide

main :: IO ()
main = do
  putStrLn $ "Millionth prime: " ++ show ((primes :: [Integer]) !! 999999)
  let n = 4356238465 * 346592384 * 59287436598 :: Integer
  putStrLn $ "Testing trial divide: " ++ show n ++ " = " ++ show (trialDivide n)
