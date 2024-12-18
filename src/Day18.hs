{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}

module Day18 (main) where

import AOC

import Data.Ord (Down(Down))
import Control.Monad (foldM, filterM, forM)
import Control.Monad.ST (ST)
import Data.Array (Array)
import Data.Array.IArray ((!))
import Data.Array.MArray as MArr
import Data.Array.IO (IOArray())
import Data.Array.Unboxed (UArray)
import Data.Array.ST
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.List (transpose, find, sortOn, group, nub, groupBy)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Control.Arrow (first)
import Debug.Trace (trace)
import Data.Foldable (foldrM)

type Coord = (Int, Int)
data Cell  = Empty | Wall Time
type Dist  = Int
type Time  = Int
type Queue = Set ((Down Time, Dist), Coord)


findPaths :: (Coord, Coord) -> Bool -> Array Coord Cell -> (Down Time, Dist)
findPaths bounds@(start, end) part2 grid = runST do
  dists <- MArr.newArray bounds (Down 0, maxBound)
  writeArray dists start (Down maxBound, 0)
  aux dists (Set.singleton ((Down maxBound, 0), start))
  readArray dists end

  where
    neighbours :: Coord -> [Coord]
    neighbours (x, y) =
      filter (inRange bounds) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

    aux :: forall s. STArray s Coord (Down Time, Dist) -> Queue -> ST s ()
    aux dists queue | Nothing <- Set.minView queue = pure ()
    aux dists queue | Just (((Down pBound, dist), p), queue) <- Set.minView queue = do
      let
        ns = flip mapMaybe (neighbours p) \c -> (c,) <$> 
          case grid ! c of
            Empty -> Just pBound
            Wall t | part2     -> t `min` pBound <$ guard (t > dist)
                   | otherwise ->         pBound <$ guard (t > 1024)

        processNeighbour :: (Coord, Time) -> Queue -> ST s Queue
        processNeighbour (n, nBound') queue = do
          nD <- readArray dists n
          let nD' = (Down nBound', dist + 1)
          if nD' >= nD then pure queue
          else do
            writeArray dists n nD'
            pure $ Set.insert (nD', n) $ Set.delete (nD, n) queue

      aux dists =<< foldrM processNeighbour queue ns

main :: IO ()
main = do
  bytes <-
    readFile "inputs/18"
      <&> strip <&> lines
      <&> mapMaybe (run $ (,) <$> decimal <* "," <*> decimal)

  let bounds = ((0, 0), (70, 70))

  grid :: IOArray Coord Cell <- newArray bounds Empty
  forM_ (zip [0..] bytes) \(k, b) -> writeArray grid b (Wall k)
  grid <- freeze grid

  -- part 1
  let (_, dist) = findPaths bounds False grid
  print dist

  -- part 2
  let (Down time, _) = findPaths bounds True grid
  print $ bytes !! time
