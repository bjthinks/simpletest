module Primes (primes) where

import SkewHeap

primes :: (Integral a) => [a]
primes = 2 : 3 : 5 : 7 : sieve (takeWhileIncreasing (spin wheel2357 11)) -- takeWhileIncreasing sucks up 0.67 seconds for first 1e6 primes

wheel2357 :: (Integral a) => [a]
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin :: (Integral a) => [a] -> a -> [a]
spin (x:xs) n = n : spin xs (n+x)

takeWhileIncreasing :: (Ord a) => [a] -> [a]
takeWhileIncreasing (x:yy@(y:_))
  | x < y = x : takeWhileIncreasing yy
  | otherwise = [x]

sieve :: (Integral a) => [a] -> [a]
sieve (x:xs) = x : sieve' xs (insertprime x xs empty)

insertprime :: (Integral a) => a -> [a] -> SkewHeap a [a] -> SkewHeap a [a]
insertprime p xs table
  | squareGood p = insert (p*p,map (*p) xs) table -- squareGood check sucks up 0.73 seconds for first 1e6 primes
  | otherwise = table

squareGood :: (Integral a) => a -> Bool
squareGood n = toInteger n^2 == toInteger (n^2)

sieve' :: (Integral a) => [a] -> SkewHeap a [a] -> [a]
sieve' [] _ = []
sieve' xx@(x:xs) table
  | head == Nothing = xx -- this check takes no discernable time
  | n <  x  = sieve' xx (adjust table)
  | n == x  = sieve' xs (adjust table)
  | otherwise = x : sieve' xs (insertprime x xs table)
  where
    head = peek table
    Just (n,_) = head

adjust :: (Integral a) => SkewHeap a [a] -> SkewHeap a [a]
adjust table
  | n > n' = restOfTable -- this check costs 0.32 seconds for first 1e6 primes
  | otherwise = insert (n',ns) restOfTable
  where
    Just ((n,n':ns),restOfTable) = pop table
