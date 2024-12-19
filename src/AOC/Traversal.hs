{-# LANGUAGE GHC2021, TypeFamilies, BlockArguments, ViewPatterns, ScopedTypeVariables, BlockArguments #-}
module AOC.Traversal
  ( Traversal(..)
  , Store(..)
  , traversal
  , traversalTo
  ) where

import AOC
import Control.Monad (when)
import Data.Array (Array)
import Data.Array.Base hiding (bounds)
import Data.Array.ST (STArray)
import Data.Foldable (foldrM)
import Data.Ix
import Data.Kind (Type, Constraint)
import Data.SequenceClass (ViewL(..))
import Data.SequenceClass qualified as Sequence
import Data.Sequence.FastQueue (FastQueue)


class Store s where
  type Ok s a :: Constraint
  type Ok s a = ()

  singleton :: a -> s a
  viewl     :: s a -> ViewL s a
  insert    :: Ok s a => a -> s a -> s a


instance Store FastQueue where
  singleton = Sequence.singleton
  viewl     = Sequence.viewl
  insert    = flip (Sequence.|>)


class Traversal t i c where
  type St t :: Type -> Type
  bounds  :: t i c -> (i, i)
  next    :: t i c -> i -> c -> [(c, i)]
  maxCost :: t i c -> c
  minCost :: t i c -> c


traversal
  :: (Ix i, Ord c, Traversal t i c, Store (St t), Ok (St t) (c, i))
  => t i c -> i -> Array i c
traversal (t :: t i c) src = runST do
  costs <- newArray (bounds t) (maxCost t)
  unsafeWrite costs (index (bounds t) src) (minCost t)
  aux costs (singleton (minCost t, src))
  freeze costs
  where
    aux :: STArray s i c -> St t (c, i) -> ST s ()
    aux costs (viewl -> EmptyL) = pure ()
    aux costs (viewl -> (c, x) :< queue) = do
      c' <- unsafeRead costs (index (bounds t) x)
      when (c <= c') $ aux costs =<< foldrM (checkNeighbour costs) queue (next t x c)

    checkNeighbour :: STArray s i c -> (c, i) -> St t (c, i) -> ST s (St t (c, i))
    checkNeighbour costs cy@(c, y) queue = do
      c' <- unsafeRead costs (index (bounds t) y)
      if (c' <= c) then pure queue
                   else do unsafeWrite costs (index (bounds t) y) c
                           pure $ insert cy queue


traversalTo
  :: (Ix i, Ord c, Traversal t i c, Store (St t), Ok (St t) (c, i))
  => t i c -> i -> i -> c
traversalTo (t :: t i c) src end = runST do
  costs <- newArray (bounds t) (maxCost t)
  unsafeWrite costs (index (bounds t) src) (minCost t)
  aux costs (singleton (minCost t, src))
  unsafeRead costs (index (bounds t) end)
  where
    aux :: STArray s i c -> St t (c, i) -> ST s ()
    aux costs (viewl -> EmptyL) = pure ()
    aux costs (viewl -> (c, x) :< queue) | x == end = pure ()
    aux costs (viewl -> (c, x) :< queue) = do
      c' <- unsafeRead costs (index (bounds t) x)
      when (c <= c') $ aux costs =<< foldrM (checkNeighbour costs) queue (next t x c)

    checkNeighbour :: STArray s i c -> (c, i) -> St t (c, i) -> ST s (St t (c, i))
    checkNeighbour costs cy@(c, y) queue = do
      c' <- unsafeRead costs (index (bounds t) y)
      if (c' <= c) then pure queue
                   else do unsafeWrite costs (index (bounds t) y) c
                           pure $ insert cy queue
