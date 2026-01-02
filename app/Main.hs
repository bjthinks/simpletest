module Main where

import Primes

main :: IO ()
main = do
  print $ (primes :: [Int]) !! 1000000
