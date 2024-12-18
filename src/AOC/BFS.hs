{-# LANGUAGE GHC2021, PatternSynonyms, ViewPatterns, BlockArguments, RecordWildCards #-}
-- | Generic utils for single-source Dijkstra traversal.
module AOC.BFS
  ( BFS(..)
  , bfs
  , bfsTo
  ) where

import AOC
import Data.Array (Array)
import Data.Array.MArray
import Data.Array.ST (STArray)
import Data.Array.Base
import Data.Foldable (foldrM)
import Data.Ix
import Data.SequenceClass (ViewL((:<), EmptyL), viewl, (|>))
import Data.Sequence.FastQueue (FastQueue)
import Data.Sequence.FastQueue qualified as FQueue


-- | Generic BFS interface.
data BFS i c = BFS
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


-- | Basic BFS
bfs :: (Ord c, Ix i) => BFS i c -> i -> Array i c
bfs (BFS{..} :: BFS i c) src = runST do
  costs <- newArray bounds maxCost
  unsafeWrite costs (index bounds src) minCost
  aux costs (FQueue.singleton (minCost, src))
  freeze costs
  where
    aux :: STArray s i c -> FastQueue (c, i) -> ST s ()
    aux costs (viewl -> EmptyL) = pure ()
    aux costs (viewl -> ((c, x) :< queue)) = -- TODO: pruning?
      aux costs =<< foldrM (checkNeighbour costs) queue (next x c)

    checkNeighbour :: STArray s i c -> (c, i) -> FastQueue (c, i) -> ST s (FastQueue (c, i))
    checkNeighbour costs cy@(c, y) queue = do
      c' <- unsafeRead costs (index bounds y)
      if (c' <= c) then pure queue
                   else do unsafeWrite costs (index bounds y) c
                           pure $ queue |> cy

-- | Basic BFS
bfsTo :: (Ord c, Ix i) => BFS i c -> i -> i -> c
bfsTo (BFS{..} :: BFS i c) src end = runST do
  costs <- newArray bounds maxCost
  unsafeWrite costs (index bounds src) minCost
  aux costs (FQueue.singleton (minCost, src))
  unsafeRead costs (index bounds end)
  where
    aux :: STArray s i c -> FastQueue (c, i) -> ST s ()
    aux costs (viewl -> EmptyL) = pure ()
    aux costs (viewl -> ((c, x) :< queue)) | x == end = pure ()
    aux costs (viewl -> ((c, x) :< queue)) = -- TODO: pruning?
      aux costs =<< foldrM (checkNeighbour costs) queue (next x c)

    checkNeighbour :: STArray s i c -> (c, i) -> FastQueue (c, i) -> ST s (FastQueue (c, i))
    checkNeighbour costs cy@(c, y) queue = do
      c' <- unsafeRead costs (index bounds y)
      if (c' <= c) then pure queue
                   else do unsafeWrite costs (index bounds y) c
                           pure $ queue |> cy
