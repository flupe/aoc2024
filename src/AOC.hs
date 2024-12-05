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
  , module Text.Megaparsec.Char.Lexer
  , module Text
  , (.:)
  , read
  , Parser
  , run
  ) where

import Prelude hiding (readFile, lines, read)
import Prelude qualified

import Control.Applicative (liftA2)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST, ST)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.Text (Text, lines, splitOn, unpack)
import Data.Text qualified as Text
import Data.Text.IO (readFile)
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
