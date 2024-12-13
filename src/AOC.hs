module AOC
  ( module Prelude
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.ST
  , module Data.Function
  , module Data.Functor
  , module Data.STRef
  , module Data.List
  , module Data.Maybe
  , module Data.Text
  , module Data.Text.IO
  , module Data.Tuple
  , module Text.Megaparsec.Char.Lexer
  , module Text
  , (.:)
  , read
  , Parser
  , run
  , pairs
  , consecutivePairs
  , middle
  , chunks
  , partitionBy
  , partitionByM
  ) where

import Prelude hiding (readFile, lines, read)
import Prelude qualified

import Control.Applicative (liftA2)
import Control.Monad (forM_, when, guard)
import Control.Monad.ST (runST, ST)
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, sortBy, stripPrefix, partition)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.Text (Text, lines, splitOn, unpack, strip)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P (parse)
import Text.Megaparsec.Char.Lexer (decimal)

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(g .: f) x = g . f x

read :: Read a => Text -> a
read = Prelude.read . Text.unpack

type Parser = Parsec Void Text

run :: Parser a -> Text -> Maybe a
run p s | Right x <- P.parse p "" s = Just x
run _ _ = Nothing

{-
mergeWith :: (a -> b -> b) -> [a] -> [b] -> [b]
mergeWith f [] ys = ys
mergeWith f xs [] = []
mergeWith f (x:xs) (y:ys) = f x y : mergeWith f xs ys

diag :: [[a]] -> [[a]]
diag [] = []
diag ([]:_) = []
diag ([xs]) = [[x] | x <- xs]
diag ((x:xs):ys) = [x] : (mergeWith (:) xs (diag ys))

-- >>> diag [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [[1],[2,4],[3,5,7],[6,8],[9]]
-- 
-}

-- | All (ordered) pairs of elements in input list.
pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

-- >>> pairs [1, 2, 3, 4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]


-- | All consecutive pairs of elements in input list.
consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = zip xs (drop 1 xs)

-- >>> cpairs [1, 2, 3, 4]
-- [(1,2),(2,3),(3,4)]


-- | Return midpoint of input list.
--   Right-biased when input has an even number of elements.
middle :: [a] -> Maybe a
middle xs = go xs xs
  where go []     _        = Nothing
        go (_:xs) (_:_:ys) = go xs ys
        go (x:_)  _        = Just x

-- >>> middle [1, 2, 3, 4, 5]
-- Just 3

-- >>> middle [1, 2, 3, 4]
-- Just 3

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- >>> chunks 3 [1 .. 7]
-- [[1,2,3],[4,5,6],[7]]

updateAssocWith :: Eq a => (b -> b -> b) -> a -> b -> [(a, b)] -> [(a, b)]
updateAssocWith f k v [] = [(k, v)]
updateAssocWith f k v ((k', v'):vs) | k == k' = (k', f v v') : vs
updateAssocWith f k v (kv:vs) = kv : updateAssocWith f k v vs

-- >>> updateAssocWith (+) 1 5 [(2, 7), (1, 1)]
-- [(2,7),(1,6)]

partitionBy :: Eq b => (a -> b) -> [a] -> [[a]]
partitionBy f xs =
    map (\x -> (f x,[x])) xs
  & foldr (uncurry $ updateAssocWith (++)) []
  & map snd

-- >>> partitionBy (`mod` 3) [1..7]
-- [[1,4,7],[3,6],[2,5]]

partitionByM :: (Monad m, Eq b) => (a -> m b) -> [a] -> m [[a]]
partitionByM f xs =
      mapM (\x -> (,[x]) <$> f x) xs
  <&> foldr (uncurry $ updateAssocWith (++)) []
  <&> map snd
