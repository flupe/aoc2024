{-# LANGUAGE GHC2021, NoImplicitPrelude, OverloadedStrings, BlockArguments, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Day18 (main) where

import AOC
import AOC.BFS
import AOC.Dijkstra
import AOC.Traversal

import Control.Applicative.Combinators (sepBy)
import Data.Ord (Down(..))
import Data.Array (Array)
import Data.Array.MArray
import Data.Array.Base (unsafeAt)
import Data.Array.IO (IOArray)
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)


type Coord  = (Int, Int)
data Cell   = Empty | Wall !Time deriving (Generic, NFData)
type Grid   = Array Coord Cell
type Dist   = Int
type Time   = Int
type Metric = (Down Time, Dist)


neighbours :: (Coord, Coord) -> Coord -> [Coord]
neighbours bounds (x, y) =
  filter (inRange bounds) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


main :: IO ()
main = timeIO "day 18" do

  (bounds@(start, end), bytes, grid :: Array Coord Cell) <- timeIO "parsing + setup" do
    bytes <-
      readFile "inputs/18"
        <&> strip
        <&> run (sepBy ((,) <$> decimal <* "," <*> decimal) "\n")
        <&> fromMaybe []

    let bounds@(start, end) = ((0, 0), (70, 70))

    grid :: IOArray Coord Cell <- newArray bounds Empty
    forM_ (zip bytes (Wall <$> [0..])) $ uncurry $ writeArray grid

    (bounds, bytes,) <$> freeze grid


  timeIO "part 1" do

    let next :: Coord -> Dist -> [Coord]
        next p d = neighbours bounds p & filter \n ->
            case unsafeAt grid (index bounds n) of
              Empty  -> True
              Wall t -> t > 1024
    print (traversalTo (BFS bounds next) start end :: Dist)


  timeIO "part 2" do

    let (getDown -> part2, _) = traversalTo dk start end
          where
            dk :: Dijkstra Coord Metric
            dk = Dijkstra bounds next maxCost minCost

            next :: Coord -> Metric -> [(Metric, Coord)]
            next p (Down t, d) =
              flip mapMaybe (neighbours bounds p) \n ->
                (,n) <$> case unsafeAt grid (index bounds n) of
                  Empty   -> Just (Down t, d + 1)
                  Wall t' -> (Down (t `min` t'), d + 1) <$ guard (t > d)

            maxCost, minCost :: Metric
            maxCost = (Down 0, maxBound)
            minCost = (Down maxBound, 0)

    print $ bytes !? part2
