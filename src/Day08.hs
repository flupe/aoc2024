{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day08 where

import AOC
import Data.Text (unpack)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Map.Strict qualified as M
import Data.Set        qualified as S
import Data.Function ( applyWhen )

type Coord = (Int, Int)
type Gen   = Coord -> Coord -> Set Coord -> Set Coord

main :: IO ()
main = do
  input <- readFile "inputs/8" <&> lines <&> map unpack

  let

    size = length input

    antennas :: Map Char [(Int, Int)] =
      foldr (\(y, line) m ->
        foldr (\(x, c) ->
          applyWhen (c /= '.') $ add c (x, y))
          m (zip [1..] line))
        M.empty (zip [1..] input)
      where add :: Char -> (Int, Int) -> Map Char [Coord] -> Map Char [Coord]
            add k v = M.alter check k
                where check Nothing   = Just [v]
                      check (Just vs) = Just (v:vs)

    inBounds :: Int -> Int -> Bool
    inBounds x y = x >= 1 && x <= size && y >= 1 && y <= size

    pin :: Coord -> Set Coord -> Set Coord
    pin c = applyWhen (uncurry inBounds c) (S.insert c)

    computeAnti1 :: Gen
    computeAnti1 (x1, y1) (x2, y2) = pin (x2 + dx, y2 + dy) . pin (x1 - dx, y1 - dy)
     where dx = x2 - x1
           dy = y2 - y1

    computeAnti2 :: Gen
    computeAnti2 (x1, _) (x2, _) cs | x1 == x2 = foldr (\y -> S.insert (x1, y)) cs [1..size]
    computeAnti2 (x1, y1) (x2, y2) cs =
      foldr (\x ->
        let ddy  = (x - x1) * dy
            y    = y1 + ddy `div` dx
        in applyWhen (ddy `rem` dx == 0) (pin (x, y))
        ) cs [1..size]
      where dx = x2 - x1
            dy = y2 - y1

    antinodes :: Gen -> S.Set Coord
    antinodes g = M.foldr' (\ps ns -> foldr (uncurry g) ns (pairs ps)) S.empty antennas

  print $ S.size $ antinodes computeAnti1
  print $ S.size $ antinodes computeAnti2
