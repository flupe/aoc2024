{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, DeriveGeneric, DeriveAnyClass #-}

module Day18 (main) where

import AOC

import Control.Applicative.Combinators (sepBy)
import Data.Ord (Down(Down, getDown))
import Data.Array (Array, (!))
import Data.Array.MArray
import Data.Array.Base (unsafeWrite, unsafeRead, unsafeAt)
import Data.Array.IO (IOArray)
import Data.Array.ST (STArray, STUArray)
import Data.Foldable (foldrM)
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.SequenceClass (ViewL((:<), EmptyL), viewl, (|>))

import Data.Sequence.FastQueue (FastQueue)
import Data.Sequence.FastQueue qualified as FQueue


type Coord  = (Int, Int)
data Cell   = Empty | Wall Time
type Grid   = Array Coord Cell
type Dist   = Int
type Time   = Int
type Metric = (Down Time, Dist)


neighbours :: (Coord, Coord) -> Coord -> [Coord]
neighbours bounds (x, y) =
  filter (inRange bounds) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


-- regular bfs
bfs :: (Coord, Coord) -> Grid -> Dist
bfs bounds@(start, end) grid = runST do
  dists <- newArray bounds maxBound
  unsafeWrite dists (index bounds start) 0
  aux dists (FQueue.singleton (0, start))
  readArray dists end
  where 
    aux :: forall s. STUArray s Coord Dist -> FastQueue (Dist, Coord) -> ST s ()
    aux dists (viewl -> EmptyL) = pure ()
    aux dists (viewl -> (pDist, p) :< queue) | p == end = pure ()
    aux dists (viewl -> (pDist, p) :< queue) = do
      pDist' <- unsafeRead dists (index bounds p)
      when (pDist' >= pDist) $
        let
          processNeighbour :: Coord -> FastQueue (Dist, Coord) -> ST s (FastQueue (Dist, Coord))
          processNeighbour n queue = do
            nDist <- unsafeRead dists (index bounds n)
            let !nDist' = pDist + 1
            if nDist' >= nDist then pure queue
            else do
              unsafeWrite dists (index bounds n) nDist'
              pure $ queue |> (nDist', n)

          ns :: [Coord]
          ns = flip filter (neighbours bounds p) \n ->
                 case unsafeAt grid (index bounds n) of
                   Empty  -> True
                   Wall t -> t > 1024
        in aux dists =<< foldrM processNeighbour queue ns


-- poor man's priority queue
type PQueue a = Set a
pattern EmptyQ   <- (Set.minView -> Nothing    )
pattern (:<:) x q <- (Set.minView -> Just (x, q))
insert :: Ord a => a -> PQueue a -> PQueue a
insert = Set.insert

-- dijkstra
dijkstra :: (Coord, Coord) -> Grid -> Time
dijkstra bounds@(start, end) grid = runST do
  dists <- newArray bounds (Down 0, maxBound)
  unsafeWrite dists (index bounds start) (Down maxBound, 0)
  aux dists (Set.singleton ((Down maxBound, 0), start))
  getDown . fst <$> unsafeRead dists (index bounds end)

  where
    aux :: forall s. STArray s Coord (Down Time, Dist)
        -> PQueue (Metric, Coord) -> ST s ()
    aux dists EmptyQ = pure ()
    aux dists (((Down !pBound, !dist), p) :<: queue) | p == end = pure ()
    aux dists ((pD'@(Down !pBound, !dist), p) :<: queue) = do
      pD <- unsafeRead dists (index bounds p)
      when (pD' <= pD) do
        let
          ns :: [(Coord, Time)]
          ns = flip mapMaybe (neighbours bounds p) \c -> (c,) <$> 
            case unsafeAt grid (index bounds c) of
              Empty  -> Just pBound
              Wall t -> t `min` pBound <$ guard (t > dist)

          processNeighbour :: (Coord, Time)
                           -> PQueue (Metric, Coord)
                           -> ST s (PQueue (Metric, Coord))
          processNeighbour (n, nBound') queue = do
            nD <- unsafeRead dists (index bounds n)
            let !nD' = (Down nBound', dist + 1)
            if nD' >= nD then pure queue
            else do
              unsafeWrite dists (index bounds n) nD'
              pure $ Set.insert (nD', n) queue

        aux dists =<< foldrM processNeighbour queue ns

main :: IO ()
main = do
  bytes <-
    readFile "inputs/18"
      <&> strip
      <&> run (sepBy ((,) <$> decimal <* "," <*> decimal) "\n")
      <&> fromMaybe []

  let bounds = ((0, 0), (70, 70))

  grid :: IOArray Coord Cell <- newArray bounds Empty
  forM_ (zip bytes (Wall <$> [0..])) $ uncurry $ writeArray grid
  grid <- freeze grid

  -- part 1
  print $ bfs bounds grid

  -- part 2
  print $ bytes !? dijkstra bounds grid
