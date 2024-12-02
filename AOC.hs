module AOC
  ( module Data.Functor
  , module Data.List
  , module Data.List.Split
  , module Data.Text
  -- , module Text.Megaparsec
  -- , Parser
  -- , parseInput
  , (.:)
  ) where

-- import Control.Applicative (Alternative)
-- import Control.Monad (MonadPlus)
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Text (Text)
-- import Data.Text.IO qualified as Text (readFile)
-- import Data.Void (Void)
-- import System.FilePath
-- import Text.Megaparsec (runParser, errorBundlePretty, Parsec, MonadParsec(..))

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(g .: f) x = g . f x

-- TODO
-- type Parser = Parsec Void Text
-- 
-- parseInput :: FilePath -> Parser a -> IO a
-- parseInput src p = do
--   input <- Text.readFile src
--   case runParser p src input of
--     Left err -> error (errorBundlePretty err)
--     Right x  -> pure x
