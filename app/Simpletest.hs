module Main where

import Primes

main :: IO ()
main = do
  putStrLn $ "Millionth prime: " ++ show ((primes :: [Int]) !! 999999)
