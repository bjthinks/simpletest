module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Writer

import TrialDivide

nl :: MaybeT (Writer String) ()
nl = tell "\n"

line :: String -> MaybeT (Writer String) ()
line str = tell str >> nl

stop :: String -> MaybeT (Writer String) ()
stop str = line str >> mzero

computeMinIndex :: Int -> Int -> Int
computeMinIndex n k
  | k `mod` n == 0 = k
  | otherwise = computeMinIndex (n `div` (gcd n k)) (k+1)

simpletest :: Int -> MaybeT (Writer String) ()
simpletest n = do
  tell $ show n ++ "\t"
  let factorization = trialDivide n
  if length factorization == 1 && snd (head factorization) == 1
    then stop "prime number"
    else return ()
  if length factorization == 1
    then stop $ "power of " ++ show (fst (head factorization))
    else return ()
  let minIndex = computeMinIndex n 3
  nl

main :: IO ()
main = putStr $ execWriter $ sequence $ map (runMaybeT . simpletest) [2..10000]
