module AOC
  ( module Data.Function
  , module Data.Functor
  , module Data.Maybe
  , module Data.List
  , module Data.List.Split
  , module Data.Text
  , module Control.Applicative
  -- , module Text.Megaparsec
  -- , parseInput
  , (.:)
  , Parser
  , run
  , int
  ) where

-- import Control.Applicative (Alternative)
-- import Control.Monad (MonadPlus)
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Control.Applicative (liftA2)
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


-- tiny monadic parser
-- TODO: replace that with megaparsec

data Parser a =
  Parser { runParser :: String -> Maybe (String, a) }
    deriving Functor

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)

  pf <*> px = Parser $ \s -> do
    (s , f) <- runParser pf s
    (s , x) <- runParser px s
    pure (s, f x)

instance Monad Parser where
  px >>= f = Parser $ \s -> do
    (s , x) <- runParser px s
    runParser (f x) s

run :: Parser a -> String -> Maybe a
run = fmap snd .: runParser

instance (a ~ ()) => IsString (Parser a) where
  fromString :: String -> Parser ()
  fromString str = Parser $ \s ->
    case str `stripPrefix` s of
      Just r  -> Just (r, ())
      Nothing -> Nothing

int :: Parser Int
int = Parser $ \s ->
  let (digits, r) = span isDigit s in
  case readsPrec 0 digits of
    (x,[]):_ -> pure (r, x)
    _        -> Nothing
