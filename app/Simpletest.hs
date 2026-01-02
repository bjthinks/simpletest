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

sylowNumbers :: Int -> [(Int,Int)] -> [[Int]]
sylowNumbers g factors = map (sylowNumber g) factors

sylowNumber :: Int -> (Int,Int) -> [Int]
sylowNumber g (p,e) =
  do let pPart = p ^ e
         pPrimePart = g `div` pPart
     candidate <- [1..pPrimePart]
     if g `mod` candidate == 0 && candidate `mod` p == 1
       then return candidate
       else []

simpletest :: Int -> MaybeT (Writer String) ()
simpletest g = do
  tell $ show g ++ "\t"
  let factorization = trialDivide g
  if length factorization == 1 && snd (head factorization) == 1
    then stop "prime number"
    else return ()
  if length factorization == 1
    then stop $ "power of " ++ show (fst (head factorization))
    else return ()
  let minIndex = computeMinIndex g 3
      sylow = sylowNumbers g factorization
  tell $ show sylow
  nl

main :: IO ()
main = putStr $ execWriter $ sequence $ map (runMaybeT . simpletest) [2..10000]
