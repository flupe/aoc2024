{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}


module Day18 (main) where

import AOC
import AOC.BFS
import AOC.Dijkstra

import Control.Applicative.Combinators (sepBy)
import Data.Ord (Down(Down))
import Data.Array (Array)
import Data.Array.MArray
import Data.Array.Base (unsafeAt)
import Data.Array.IO (IOArray)
import Data.List ((!?))
import Data.Maybe (fromMaybe)


type Coord  = (Int, Int)
data Cell   = Empty | Wall Time
type Grid   = Array Coord Cell
type Dist   = Int
type Time   = Int
type Metric = (Down Time, Dist)


neighbours :: (Coord, Coord) -> Coord -> [Coord]
neighbours bounds (x, y) =
  filter (inRange bounds) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


main :: IO ()
main = do
  bytes <-
    readFile "inputs/18"
      <&> strip
      <&> run (sepBy ((,) <$> decimal <* "," <*> decimal) "\n")
      <&> fromMaybe []

  let bounds@(start, end) = ((0, 0), (70, 70))

  grid :: IOArray Coord Cell <- newArray bounds Empty
  forM_ (zip bytes (Wall <$> [0..])) $ uncurry $ writeArray grid
  grid :: Grid <- freeze grid

  let
    part1 :: Dist
    part1 = bfsTo (BFS bounds next maxBound 0) start end
      where
        next :: Coord -> Dist -> [(Dist, Coord)]
        next p d =
          flip mapMaybe (neighbours bounds p) \n ->
            (,n) <$> case unsafeAt grid (index bounds n) of
              Empty  -> Just (d + 1)
              Wall t -> (d + 1) <$ guard (t > 1024)

    (Down part2, _) = dijkstraTo (Dijkstra bounds next maxCost minCost) start end
      where
        next :: Coord -> Metric -> [(Metric, Coord)]
        next p (Down t, d) =
          flip mapMaybe (neighbours bounds p) \n ->
            (,n) <$> case unsafeAt grid (index bounds n) of
              Empty   -> Just (Down t, d + 1)
              Wall t' -> (Down (t `min` t'), d + 1) <$ guard (t > d)

        maxCost, minCost :: Metric
        maxCost = (Down 0, maxBound)
        minCost = (Down maxBound, 0)

  print $ part1
  print $ bytes !? part2
