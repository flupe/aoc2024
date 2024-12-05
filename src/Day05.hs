{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day05 where

import AOC

import Data.Tuple (swap)
import Data.Ord (Ordering)
import Data.List (sortBy, partition)

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

-- >>> pairs [1, 2, 3, 4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

middle :: [a] -> Maybe a
middle xs = go xs xs
  where go []     _        = Nothing
        go (_:xs) (_:_:ys) = go xs ys
        go (x:_)  _        = Just x

-- >>> middle [1, 2, 3, 4, 5]
-- Just 3

main :: IO ()
main = do
  [one, two] <- readFile "inputs/5" <&> splitOn "\n\n"

  let
    rules :: [(Int, Int)]
    rules = lines one & mapMaybe (run $ (,) <$> decimal <* "|" <*> decimal)

    updates :: [[Int]]
    updates = lines two & map (splitOn ",") & map (map read)

    isValid :: [Int] -> Bool
    isValid xs = all (not . (`elem` rules) . swap) (pairs xs)

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
