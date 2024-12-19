{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, DeriveGeneric, DeriveAnyClass #-}

module Day19 where

import AOC

import Data.Array (Array)
import Data.Array.IArray
import Data.List (stripPrefix, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Trie = Node !Bool (Map Char Trie) deriving (Generic, NFData)

empty :: Trie
empty = Node False Map.empty

insert :: String -> Trie -> Trie
insert []     (Node b m) = Node True m
insert (c:cs) (Node b m) = Node b (Map.alter (Just . insert cs . fromMaybe empty) c m)

count :: Trie -> String -> Int
count t src = dropped ! 0
  where
    dropped :: Array Int Int
    dropped = genArray (0, length src - 1) \i -> dropPrefix t (drop i src) i

    dropPrefix :: Trie -> String -> Int -> Int
    dropPrefix (Node b m) [] k = if b then 1 else 0
    dropPrefix (Node b m) (x:xs) !k =
      let now = if b then dropped ! k else 0 in
      case Map.lookup x m of
        Just t  -> now + dropPrefix t xs (k + 1)
        Nothing -> now

main :: IO ()
main = timeIO "total day 1" do

  (trie, patterns) :: (Trie, [String]) <- timeIO "parsing + preprocess" do

    [ttowels, tpatterns] <- readFile "inputs/19" <&> strip <&> splitOn "\n\n"

    let towels       :: [String] = map unpack $ splitOn ", " ttowels
        patterns     :: [String] = map unpack $ splitOn "\n" tpatterns

    pure (foldr insert empty towels, patterns)


  timeIO "part 1 + part2" do

    let combinations :: [Int]    = filter (> 0) $ map (count trie) patterns

    print $ length $ combinations
    print $ sum combinations
