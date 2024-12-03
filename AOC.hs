module AOC
  ( module Prelude
  , module Data.Function
  , module Data.Functor
  , module Data.List
  , module Data.Maybe
  , module Data.Text
  , module Data.Text.IO
  , module Control.Applicative
  , module Text.Megaparsec.Char.Lexer
  , (.:)
  , read
  , Parser
  , run
  ) where

import Prelude hiding (readFile, lines, read)
import Prelude qualified

import Control.Applicative (liftA2)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text, lines, splitOn)
import Data.Text qualified as T
import Data.Text.IO (readFile)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P (parse)
import Text.Megaparsec.Char.Lexer (decimal)

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(g .: f) x = g . f x

read :: Read a => Text -> a
read = Prelude.read . T.unpack

type Parser = Parsec Void Text

run :: Parser a -> Text -> Maybe a
run p s | Right x <- P.parse p "" s = Just x
run _ _ = Nothing
