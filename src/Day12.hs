{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, BlockArguments #-}

module Day12 where

import AOC
import Control.Monad (filterM)
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Array.IArray (listArray, bounds, (!))
import Data.Array.Unboxed (UArray)

type Coord = (Int, Int)
data Farm = Farm { size :: Int, grid :: UArray Coord Char }

-- basic union find in ST

data Union s = Union
  { roots  :: STArray  s Coord Coord
  , sizes  :: STUArray s Coord Int
  , perims :: STUArray s Coord Int
  }

newUnion :: (Int, Int) -> ST s (Union s)
newUnion (h, w) =
  Union <$> newListArray r (range  r) 
        <*> newArray r 1 
        <*> newArray r 0
  where r = ((1, 1), (h, w))

find :: Union s -> Coord -> ST s Coord
find u@Union{..} c = do
  p <- readArray roots c
  if p == c then pure p
  else do
    r <- find u p
    writeArray roots c r
    pure r

isRoot :: Union s -> Coord -> ST s Bool
isRoot u x = (x==) <$> find u x

union :: Union s -> Coord -> Coord -> ST s ()
union u@Union{..} x y = do
  x <- find u x
  y <- find u y
  when (x /= y) do
    sx <- readArray sizes x
    sy <- readArray sizes y
    px <- readArray perims x
    py <- readArray perims y
    if sx < sy then do
      writeArray roots  x y
      writeArray sizes  y (sx + sy)
      writeArray perims y (px + py)
    else do
      writeArray roots  y x
      writeArray sizes  x (sx + sy)
      writeArray perims x (px + py)

--------------------


cost :: Farm -> Int
cost Farm{..} = runST do
  u@Union{..} <- newUnion (size, size)

  -- find plots
  forM_ (range ((1, 1), (size, size))) \p -> do

    let plot = grid ! p
    let ns = filter (inPlot plot) (neighbours p)
    
    writeArray perims p (4 - length ns)
    -- we're not counting perims now, but corners

    forM_ (filter (<= p) ns) $ union u p

  -- compute cost
  rs  <- filterM (isRoot u) $ range ((1, 1), (size, size))
  tot <- newSTRef 0

  forM_ rs \p -> do
    perim <- readArray perims p
    area  <- readArray sizes  p
    modifySTRef' tot (+ (perim * area))

  readSTRef tot

  where
    neighbours :: Coord -> [Coord]
    neighbours (y, x) =
      filter (inRange $ bounds grid)
        [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

    inPlot :: Char -> Coord -> Bool
    inPlot c = (c ==) . (grid !)

main :: IO ()
main = do
  input <- readFile "inputs/12" <&> lines <&> map unpack <&> concat

  -- let 
  --    input :: [Char] =
  --      "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
  --      & lines & map unpack & concat

  let size :: Int = floor $ sqrt $ fromIntegral $ length input -- assuming square input
      farm = Farm size $ listArray ((1, 1), (size, size)) input

  print $ cost farm
