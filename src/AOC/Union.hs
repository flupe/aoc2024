{-# LANGUAGE BlockArguments, RecordWildCards #-}
module AOC.Union 
  ( Union
  , new
  , find
  , isRoot
  , applyAnn
  , getSize
  , getAnn
  , union
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Ix (Ix, range)
import Data.Array.ST
import Data.Array.IArray (listArray, bounds, (!))
import Data.Array.Unboxed (UArray)


-- | Data structure for union-find operations.
data Union s i a = Union
  { roots  :: STArray  s i i
  , sizes  :: STUArray s i Int
  , anns   :: STArray  s i a
  }

-- | Create a new union-find structure with indices in the given range,
-- with all sets annotated with a given initial value.
new :: Ix i => (i, i) -> a -> ST s (Union s i a)
new r x = Union <$> newListArray r (range r)
                <*> newArray r 1
                <*> newArray r x

-- | Find the root of the set a given index belongs to.
find :: Ix i => Union s i a -> i -> ST s i
find u@Union{..} c = do
  p <- readArray roots c
  if p == c then pure p
  else do
    r <- find u p
    writeArray roots c r
    pure r

-- | Check if an index is the root of its set.
isRoot :: Ix i => Union s i a -> i -> ST s Bool
isRoot u x = (x ==) <$> find u x

-- | Update the annotation of a given set.
applyAnn :: (Ix i, Semigroup a) => Union s i a -> i -> a -> ST s ()
applyAnn u@Union{..} x m = do
  x <- find u x
  modifyArray anns x (<> m)

-- | Retrieve the size of a given set.
getSize :: Ix i => Union s i a -> i -> ST s Int
getSize u@Union{..} x = find u x >>= readArray sizes

-- | Retrieve the annotation of a given set.
getAnn :: Ix i => Union s i a -> i -> ST s a
getAnn u@Union{..} x = find u x >>= readArray anns

-- | Merge two sets, using the semigroup operation for annotations.
union :: (Ix i, Semigroup a) => Union s i a -> i -> i -> ST s ()
union u@Union{..} x y = do
  x <- find u x
  y <- find u y
  when (x /= y) do
    sx <- readArray sizes x
    sy <- readArray sizes y
    ax <- readArray anns x
    ay <- readArray anns y
    if sx < sy then do
      writeArray roots  x y
      writeArray sizes  y (sx + sy)
      writeArray anns y (ax <> ay)
    else do
      writeArray roots  y x
      writeArray sizes  x (sx + sy)
      writeArray anns x (ax <> ay)
