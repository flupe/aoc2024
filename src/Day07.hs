{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day07 where

import AOC
import Control.Applicative (some, many)
import Data.IntSet qualified as IntSet
-- import Data.Set    qualified as Set


validTotal1, validTotal2 :: Int -> [Int] -> Bool

validTotal1 tot (one:items) =
  IntSet.member tot $ foldl' add (IntSet.singleton one) items
  where add set item =(IntSet.map (item +) set) `IntSet.union` (IntSet.map (item *) set)

validTotal2 tot (one:items) =
  IntSet.member tot $ foldl' add (IntSet.singleton one) items
  where add set item =   (IntSet.map (item +) set)
          `IntSet.union` (IntSet.map (item *) set)
          `IntSet.union` (IntSet.map ((item +) . (* bound10 item)) set)

        bound10 :: Int -> Int
        bound10 x | x < 10    = 10
                  | otherwise = 10 * bound10 (x `div` 10)

main :: IO ()
main = do
  input <-
    readFile "inputs/7" 
    <&> lines
    <&> mapMaybe (run $ (,) <$> decimal <* ":" <*> some (" " *> decimal))

  let 
    (part1, rest) = partition (uncurry validTotal1) input
    part2         = filter (uncurry validTotal2) rest

    r1 = sum $ map fst part1
    r2 = sum $ map fst part2

  print $ r1
  print $ r1 + r2
