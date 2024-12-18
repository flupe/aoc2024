{-# LANGUAGE GHC2021, PatternSynonyms, ViewPatterns, BlockArguments, RecordWildCards #-}
-- | Generic utils for single-source Dijkstra traversal.
module AOC.Dijkstra
  ( Dijkstra(..)
  , dijkstra
  , dijkstraTo
  ) where

import AOC
import Data.Array (Array)
import Data.Array.MArray
import Data.Array.ST (STArray)
import Data.Array.Base
import Data.Foldable (foldrM)
import Data.Ix
import Data.Set (Set)
import Data.Set qualified as Set


-- poor man's priority queue
type PQueue a = Set a
pattern EmptyQ   <- (Set.minView -> Nothing    )
pattern (:<:) x q <- (Set.minView -> Just (x, q))
insert :: Ord a => a -> PQueue a -> PQueue a
insert = Set.insert


-- | Generic single-source Dijkstra interface
data Dijkstra i c = Dijkstra
  { -- | Bounds the vertices range over.
    bounds  :: (i, i)

    -- | For a given cell with cost @c@, compute neighbours and their associated cost.
    -- The cost of each neighbour *must* be larger than the one of the input vertex.
  , next    :: i -> c -> [(c, i)]

    -- | Maximal cost for unvisited vertices.
  , maxCost :: c

    -- | Initial cost for the source.
  , minCost :: c
  }


-- TODO: find a better priority queue implementation

-- | Single-source generic dijkstra.
dijkstra :: (Ord c, Ix i) => Dijkstra i c -> i -> Array i c
dijkstra (Dijkstra{..} :: Dijkstra i c) src = runST do
  costs <- newArray bounds maxCost
  unsafeWrite costs (index bounds src) minCost
  aux costs (Set.singleton (minCost, src))
  freeze costs
  where
    aux :: STArray s i c -> PQueue (c, i) -> ST s ()
    aux costs EmptyQ = pure ()
    aux costs ((c, x) :<: queue) = -- TODO: pruning?
      aux costs =<< foldrM (checkNeighbour costs) queue (next x c)

    checkNeighbour :: STArray s i c -> (c, i) -> PQueue (c, i) -> ST s (PQueue (c, i))
    checkNeighbour costs cy@(c, y) queue = do
      c' <- unsafeRead costs (index bounds y)
      if (c' <= c) then pure queue
                   else do unsafeWrite costs (index bounds y) c
                           pure $ insert cy queue


-- | Single-source generic dijkstra with early cutoff at target vertex.
dijkstraTo :: (Ord c, Ix i) => Dijkstra i c -> i -> i -> c
dijkstraTo (Dijkstra{..} :: Dijkstra i c) src end = runST do
  costs <- newArray bounds maxCost
  unsafeWrite costs (index bounds src) minCost
  aux costs (Set.singleton (minCost, src))
  unsafeRead costs (index bounds end)
  where
    aux :: STArray s i c -> PQueue (c, i) -> ST s ()
    aux costs EmptyQ = pure ()
    aux costs ((c, x) :<: queue) | x == end = pure ()
    aux costs ((c, x) :<: queue) = -- TODO: pruning?
      aux costs =<< foldrM (checkNeighbour costs) queue (next x c)

    checkNeighbour :: STArray s i c -> (c, i) -> PQueue (c, i) -> ST s (PQueue (c, i))
    checkNeighbour costs cy@(c, y) queue = do
      c' <- unsafeRead costs (index bounds y)
      if (c' <= c) then pure queue
                   else do unsafeWrite costs (index bounds y) c
                           pure $ insert cy queue
