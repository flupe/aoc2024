{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day05 where

import AOC

import Data.Ord (Ordering)

main :: IO ()
main = do
  [one, two] <- readFile "inputs/5" <&> splitOn "\n\n"

  let
    rules :: [(Int, Int)]
    rules = lines one & mapMaybe (run $ (,) <$> decimal <* "|" <*> decimal)

    updates :: [[Int]]
    updates = lines two & map (splitOn ",") & map (map read)

    isValid :: [Int] -> Bool
    isValid = not . any ((`elem` rules) . swap) . consecutivePairs

    cmp :: Int -> Int -> Ordering
    cmp x y | (x, y) `elem` rules = LT
    cmp x y | (y, x) `elem` rules = GT
    cmp _ _ | otherwise           = EQ -- ???

    valid, invalid, fixed :: [[Int]]
    (valid, invalid) = partition isValid updates
    fixed            = map (sortBy cmp) invalid

    answer :: [[Int]] -> IO ()
    answer = print . sum . mapMaybe middle

  answer valid
  answer fixed
