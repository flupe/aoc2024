{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, BlockArguments #-}

module Day12 where

import AOC
import AOC.Union as Union

import Control.Monad (filterM)
import Control.Monad.ST (ST)
import Data.STRef
import Data.Array.ST
import Data.Array.IArray (listArray, bounds, (!))
import Data.Array.Unboxed (UArray)
import Data.Semigroup (Sum(..))

type Coord = (Int, Int)
data Farm  = Farm { size :: Int, grid :: UArray Coord Char }

cost :: Farm -> Int
cost Farm{..} = runST do
  let r = ((1, 1), (size, size))

  u <- Union.new r (Sum 0)

  -- find plots
  forM_ (range r) \p -> do

    let plot = grid ! p
    let ns = filter (inPlot plot) (neighbours p)
    
    applyAnn u p (Sum $ 4 - length ns)
    -- we're not counting perims now, but corners

    forM_ (filter (<= p) ns) $ union u p

  -- compute cost
  rs  <- filterM (isRoot u) $ range ((1, 1), (size, size))
  tot <- newSTRef 0

  forM_ rs \p -> do
    Sum perim <- getAnn  u p
    area      <- getSize u p
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
