{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ViewPatterns #-}

module Day10 where

import AOC
import Data.Foldable (foldMap')
import Control.Monad (foldM, forM_)
import Data.Array.IArray as A
import Data.Array.Unboxed (UArray)
import Data.Text (unpack, strip)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Function (applyWhen)
import Data.Ix (inRange)

type Coord = (Int, Int)
type HeightMap = UArray Coord Char


trailheads :: HeightMap -> [Set [Coord]]
trailheads hmap = aux "876543210" nines
  where
    nines :: Map Coord (Set [Coord]) =
      foldr (\k -> applyWhen (hmap A.! k == '9') $ M.insert k (S.singleton [k]))
        M.empty $ A.indices hmap

    neighbours :: Coord -> [Coord]
    neighbours (y, x) =
      filter (inRange $ A.bounds hmap) [ (y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1) ]

    aux :: [Char] -> Map Coord (Set [Coord]) -> [Set [Coord]]
    aux []     !st = M.elems st
    aux (v:vs) !st = aux vs $ M.foldrWithKey checkN M.empty st
      where checkN k c r =
              foldr (\k -> applyWhen (hmap A.! k == v) $ M.insertWith S.union k (S.map (k:) c))
                r (neighbours k)

main :: IO ()
main = do
  txt <- readFile "inputs/10" <&> strip <&> lines <&> map unpack <&> concat

  let size    = floor $ sqrt $ fromIntegral $ length txt
      hmap    = listArray ((1, 1), (size, size)) txt
      !theads = trailheads hmap
      score m = sum $ map m $ theads

  print $ score $ S.size . S.map last
  print $ score $ S.size
