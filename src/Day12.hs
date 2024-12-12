{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

module Day12 where

import AOC
import AOC.Union as Union

import Data.Foldable (foldMap')
import Data.Bifunctor (bimap)
import Control.Monad (filterM, foldM, forM)
import Control.Monad.ST (ST)
import Data.STRef
import Data.List (groupBy)
import Data.Array.ST
import Data.Array.IArray (listArray, bounds, (!))
import Data.Array.Unboxed (UArray)
import Data.Semigroup (Sum(..))
import Debug.Trace (traceShow)

type Coord    = (Int, Int)
data Farm     = Farm { size :: Int, grid :: UArray Coord Char }
type Union' s = Union s Coord (Sum Int, Sum Int)

costs :: Farm -> (Int, Int)
costs Farm{..} = runST do
  u :: Union' s <- Union.new bounds (Sum 0, Sum 0)

  -- find plots & perimeters
  forM_ (range bounds) \p -> do
    let plot = grid ! p
        ns   = filter (inPlot plot) $ neighbours p
    forM_ (filter (<= p) ns) $ union u p
    addPerim u p $ 4 - length ns

  -- corner processing
  forM_ (range ((0, 0), (size, size))) \c -> do

    -- find plots surrounding corner, grouped by plant type
    cs <- cornering c & partitionByM (find u)

    forM_ cs \case
      [x]                 -> addCorners u x 1
      [x, y] | inDiag x y -> addCorners u x 2
      [x, y, z]           -> addCorners u x 1
      _                   -> pure ()

  -- compute costs
  plots <- filterM (isRoot u) $ range bounds

  (Sum cost1, Sum cost2) <- foldMap' id <$> forM plots \p -> do
    area <- Sum <$> getSize u p
    bimap (* area) (* area) <$> getAnn u p

  pure (cost1, cost2)

  where
    inDiag :: Coord -> Coord -> Bool
    inDiag (y1, x1) (y2, x2) = y1 /= y2 && x1 /= x2

    bounds :: (Coord, Coord)
    bounds = ((1, 1), (size, size))

    addPerim, addCorners :: Union' s -> Coord -> Int -> ST s ()
    addPerim   u p x = applyAnn u p (Sum x, Sum 0)
    addCorners u p x = applyAnn u p (Sum 0, Sum x)

    neighbours :: Coord -> [Coord]
    neighbours (y, x) =
      filter (inRange bounds)
        [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

    cornering :: Coord -> [Coord]
    cornering (y, x) =
      filter (inRange bounds)
        [(y, x), (y + 1, x), (y, x + 1), (y + 1, x + 1)]

    inPlot :: Char -> Coord -> Bool
    inPlot c = (c ==) . (grid !)

main :: IO ()
main = do
  input <- readFile "inputs/12" <&> lines <&> map unpack

  let size = length input -- assuming square input
      farm = Farm size $ listArray ((1, 1), (size, size)) $ concat input

  print $ costs farm
