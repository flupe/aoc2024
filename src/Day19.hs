{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day19 where



import AOC
import Data.Ord (Down(Down))

import Data.List (stripPrefix, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)


data Trie = Node Bool (Map Char Trie) deriving Show

empty :: Trie
empty = Node False Map.empty

insert :: String -> Trie -> Trie
insert []     (Node b m) = Node True m
insert (c:cs) (Node b m) = Node b (Map.alter (Just . insert cs . fromMaybe empty) c m)

valid :: Trie -> String -> Int
valid t src = dropped !! 0
  where
    dropped :: [Int]
    dropped = [dropPrefix t (drop i src) i | i <- [0 .. length src - 1]]

    dropPrefix :: Trie -> String -> Int -> Int
    dropPrefix (Node b m) [] k = if b then 1 else 0
    dropPrefix (Node b m) (x:xs) !k =
      let now = if b then dropped !! k else 0 in
      case Map.lookup x m of
        Just t  -> now + dropPrefix t xs (k + 1)
        Nothing -> now

main :: IO ()
main = do
  [ttowels, tpatterns] <- readFile "inputs/19" <&> strip <&> splitOn "\n\n"

  let towels   :: [String] = sortOn (Down . length) $ map unpack $ splitOn ", " ttowels
      patterns :: [String] = map unpack $ splitOn "\n" tpatterns

  let trie         :: Trie     = foldr insert empty towels
  let combinations :: [Int] = map (valid trie) patterns

  print $ length $ filter (> 0) combinations
  print $ sum combinations
