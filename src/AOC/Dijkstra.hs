{-# LANGUAGE GHC2021, PatternSynonyms, ViewPatterns, BlockArguments, RecordWildCards, DerivingVia, DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic utils for single-source Dijkstra traversal.
module AOC.Dijkstra
  ( Dijkstra(..)
  -- , dijkstra
  -- , dijkstraTo
  ) where


import AOC
import AOC.Traversal

import Data.Array (Array)
import Data.Array.MArray
import Data.Array.ST (STArray)
import Data.Array.Base
import Data.Foldable (foldrM)
import Data.Ix
import Data.Set (Set)
import Data.Set qualified as Set
import Data.SequenceClass


-- poor man's priority queue
newtype PQueue a = PQueue (Set a)

instance Store PQueue where
  type Ok PQueue a = Ord a
  singleton = PQueue . Set.singleton
  viewl (PQueue s) =
    case Set.minView s of
      Nothing     -> EmptyL
      Just (x, s) -> x :< PQueue s
  insert x (PQueue s) = PQueue (Set.insert x s)

-- | Generic single-source Dijkstra interface
data Dijkstra i c = Dijkstra
  { -- | Bounds the vertices range over.
    dkBounds  :: (i, i)

    -- | For a given cell with cost @c@, compute neighbours and their associated cost.
    -- The cost of each neighbour *must* be larger than the one of the input vertex.
  , dkNext    :: i -> c -> [(c, i)]

    -- | Maximal cost for unvisited vertices.
  , dkMaxCost :: c

    -- | Initial cost for the source.
  , dkMinCost :: c
  }

instance Traversal Dijkstra i c where
  type St Dijkstra = PQueue
  bounds  = dkBounds
  next    = dkNext
  maxCost = dkMaxCost
  minCost = dkMinCost


-- TODO: find a better priority queue implementation
