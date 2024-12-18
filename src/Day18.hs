{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Day18 (main) where

import AOC

import Data.Ord (Down(Down))
import Data.Array (Array, (!))
import Data.Array.MArray
import Data.Array.IO (IOArray)
import Data.Array.ST (STArray)
import Data.Foldable (foldrM)
import Data.Set (Set)
import Data.Set qualified as Set

type Coord  = (Int, Int)
data Cell   = Empty | Wall Time
type Dist   = Int
type Time   = Int
type Metric = (Down Time, Dist)

-- poor man's priority queue
type Queue a = Set a
pattern EmptyQ   <- (Set.minView -> Nothing    )
pattern (:<) x q <- (Set.minView -> Just (x, q))
insert :: Ord a => a -> Queue a -> Queue a
insert = Set.insert

-- dijkstra
findPaths :: (Coord, Coord) -> Bool -> Array Coord Cell -> (Down Time, Dist)
findPaths bounds@(start, end) part2 grid = runST do
  dists <- newArray bounds (Down 0, maxBound)
  writeArray dists start (Down maxBound, 0)
  aux dists (Set.singleton ((Down maxBound, 0), start))
  readArray dists end

  where
    neighbours :: Coord -> [Coord]
    neighbours (x, y) =
      filter (inRange bounds) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

    aux :: forall s. STArray s Coord (Down Time, Dist)
        -> Queue (Metric, Coord) -> ST s ()
    aux dists EmptyQ = pure ()
    aux dists (((Down pBound, dist), p) :< queue) | p == end = pure ()
    aux dists (((Down pBound, dist), p) :< queue) = do
      let
        -- neighbours and how long they last
        ns :: [(Coord, Time)]
        ns = flip mapMaybe (neighbours p) \c -> (c,) <$> 
          case grid ! c of
            Empty -> Just pBound
            Wall t | part2     -> t `min` pBound <$ guard (t > dist)
                   | otherwise ->         pBound <$ guard (t > 1024)

        processNeighbour :: (Coord, Time)
                         -> Queue (Metric, Coord)
                         -> ST s (Queue (Metric, Coord))
        processNeighbour (n, nBound') queue = do
          nD <- readArray dists n
          let nD' = (Down nBound', dist + 1)
          if nD' >= nD then pure queue
          else do
            writeArray dists n nD'
            pure $ Set.insert (nD', n) queue

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
