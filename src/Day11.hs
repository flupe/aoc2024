{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day11 where

import AOC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Store = Map (Int, Int) Int

digits :: Int -> Int
digits x | x < 10    = 1
         | otherwise = 1 + digits (x `div` 10)


stones :: Store -> Int -> Int -> (Int, Store)
stones !st !sn !k | k <= 0 = (1, st)
stones !st !sn !k | Just n <- st Map.!? (sn, k) = (n, st)
stones !st 0   !k =
  let (y, st') = stones st 1 (k - 1)
   in (y, Map.insert (0, k) y st')
stones !st !x  !k | ds <- digits x, even ds =
  let mask = 10 ^ (ds `div` 2)
      (y, !st')  = stones st  (x `div` mask) (k - 1)
      (z, !st'') = stones st' (x `mod` mask) (k - 1)
  in (y + z, Map.insert (x, k) (y + z) st'')
stones !st !x !k =
  let (y, st') = stones st (x * 2024) (k - 1)
   in (y, Map.insert (x, k) y st')


manyStones :: [Int] -> Int -> Int
manyStones ss n = fst $ foldl' step (0, Map.empty) ss
  where step (s, st) x = let (r, st') = stones st x n in (s + r, st')


main :: IO ()
main = do
  input :: [Int] <- readFile "inputs/11" <&> splitOn " " <&> map read

  print $ manyStones input 25
  print $ manyStones input 75
