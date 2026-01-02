module Main where

import Primes

main :: IO ()
main = sequence_ $ map (putStrLn . show) (primes :: [Int])
