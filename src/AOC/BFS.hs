{-# LANGUAGE GHC2021, PatternSynonyms, ViewPatterns, BlockArguments, RecordWildCards, TypeFamilies #-}
-- | Generic utils for single-source Dijkstra traversal.
module AOC.BFS
  ( BFS(..)
  ) where

import AOC
import AOC.Traversal

import Data.Array (Array)
import Data.Array.MArray
import Data.Array.ST (STArray)
import Data.Array.Base
import Data.Foldable (foldrM)
import Data.Ix
import Data.SequenceClass (ViewL((:<), EmptyL), viewl, (|>))
import Data.Sequence.FastQueue (FastQueue)
import Data.Sequence.FastQueue qualified as FQueue


-- | Basic BFS interface.
data BFS i c = BFS
  { -- | Bounds the vertices range over.
    bfsBounds  :: (i, i)
    -- | For a given cell at a given distance, compute direct neighbours.
  , bfsNext    :: i -> Int -> [i]
  }

instance Traversal BFS i Int where
  type St BFS = FastQueue
  bounds      = bfsBounds
  next t x d  = (d + 1,) <$> bfsNext t x d
  maxCost     = const maxBound
  minCost     = const 0
