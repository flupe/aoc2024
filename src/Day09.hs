{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day09 where

import AOC hiding (read)
import Control.Applicative (some)
import Prelude (read)
import Data.List (singleton)
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Text (strip)

data Block = Files Int Int | Empty Int deriving (Eq, Show)
type Disk = [Block]

isEmpty :: Block -> Bool
isEmpty (Empty _) = True
isEmpty _ = False

expand :: [Int] -> (Disk, Int)
expand ls = (go 0 ls, (length ls - 1) `div` 2)
  where go :: Int -> [Int] -> Disk
        go id [] = []
        go id [files] = [Files id files]
        go id (files:free:rest) = Files id files : Empty free : go (id + 1) rest

showDisk :: Disk -> String
showDisk [] = ""
showDisk (Empty e   :d) = replicate e '.' ++ showDisk d
showDisk (Files id l:d) = concat (replicate l (show id)) ++ showDisk d

add :: Disk -> Disk -> Disk
add []                   ys   = ys
add xs                   []   = xs
add xs                   (Empty _:ys)   = add xs ys
add (b@(Files _ _):xs)   ys             = b : add xs ys
add (Empty e      :xs)   (Files i l:ys)
    | e <  l    = Files i e : add xs (Files i (l - e) : ys)
    | e == l    = Files i l : add xs ys
    | otherwise = Files i l : add (Empty (e - l) : xs) ys

size :: Disk -> Int
size [] = 0
size (Files _ l:d) = l + size d
size (Empty _:d)   = size d

take' :: Int -> Disk -> Disk
take' k []         = []
take' k _ | k <= 0 = []
take' k (Files i l:d) = Files i (l `min` k) : take' (k - l) d
take' k (Empty e  :d) = Empty (e `min` k) : take' k d

toMap :: Disk -> M.Map Int Int
toMap []             = M.empty
toMap (Empty _:d)    = toMap d
toMap (Files id l:d) = M.insert id l $ toMap d

rearrange :: Disk -> Int -> Disk
rearrange d maxId =
  foldr insert d [0 .. maxId]
  where 
    insert :: Int -> Disk -> Disk
    insert id = go
      where go [] = []
            go (Empty e:Empty e':d)  = go (Empty (e + e') : d)
            go (Empty e:d) | e >=  l = Files id l : if e > l then (Empty (e - l) : drop d) else drop d
            go (Empty e:d)           = Empty e : go d
            go d@(Files i r : d') = if i == id then d else Files i r : go d'

            drop [] = []
            drop (Empty e   :d) = Empty e : drop d
            drop (Files i _ :d) | i == id = Empty l : d
            drop (Files i l':d) = Files i l' : drop d

            l = files M.! id

    !files = toMap d

checksum :: Disk -> Int
checksum = go 0
  where go !p [] = 0
        go !p (Empty e   :d) = go (p + e) d
        go !p (Files id l:d) | l > 0 = p * id + go (p + 1) (Files id (l - 1) : d)
        go !p (Files id l:d) = go p d

main :: IO ()
main = do
  input :: [Int] <- readFile "inputs/9" <&> strip <&> unpack <&> map (read . singleton)

  let (disk, maxId) = expand input

  print $ checksum $ rearrange disk maxId
