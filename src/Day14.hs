{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments #-}

module Day14 where

import Text.Megaparsec.Char.Lexer (signed)
import Control.Monad (forM)
import Data.List (group)
import AOC
import Data.Array.IO
import Data.Array.MArray

int = signed mempty decimal

type Coord = (Int, Int)
type Speed = (Int, Int)
type Bot   = (Coord, Speed)

move :: (Int, Int) -> Int -> Bot -> Coord
move (width, height) dt ((px, py), (vx, vy)) =
  ( (px + vx * dt) `mod` width
  , (py + vy * dt) `mod` height
  )

quad :: (Int, Int) -> Coord -> Maybe (Bool, Bool)
quad (width, height) (x, y) = (x < hw, y < hh) <$ guard (x /= hw && y /= hh)
    where hw = (width  - 1) `div` 2
          hh = (height - 1) `div` 2

main :: IO ()
main = do
  bots <-
    readFile "inputs/14"
      <&> strip 
      <&> lines 
      <&> mapMaybe (run $ (,) <$> ((,) <$ "p=" <*> int <* "," <*> int) <* " "
                              <*> ((,) <$ "v=" <*> int <* "," <*> int))

  let size = (101, 103)

  -- part 1
  map (move size 100) bots
    & mapMaybe (quad size)
    & sort
    & group
    & map length
    & foldr1 (*)
    & print

  -- awful part 2
  let r = ((0, 0), (100, 102))

  grid :: IOArray Coord Int <- newArray r 0
  forM_ bots \(p, _) -> modifyArray grid p succ

  let
    showGrid :: IO ()
    showGrid = forM_ [0..102] \y -> do
      line :: [String] <- forM ((,y) <$> [0..100]) \p -> do
        c <- readArray grid p
        pure $ if c == 0 then "  " else "XX"
      putStrLn $ concat line

    -- ad-hoc score encouraging filled areas
    gridScore :: IO Int 
    gridScore = do
      sum <$> forM (range r) \p -> 
        (*) <$> readArray grid p 
            <*> (sum <$> forM (neighbours p) (readArray grid))
      where neighbours :: Coord -> [Coord]
            neighbours (y, x) = filter (inRange r) [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

    findGoodCandidate :: Int -> [Bot] -> IO ()
    findGoodCandidate !count bots = do
        -- move bots around
        bots <- forM bots \b@(p@(px, py), v) ->
          let p' = move size 1 b
          in (p', v) <$ modifyArray grid p' succ 
                     <* modifyArray grid p pred

        score <- gridScore

        if score > 350 then do -- arbitrary threshold to filter out "random" grids
          putStrLn $ "After " <> show count <> " step(s):"
          showGrid
          () <$ getChar
        else putStrLn $ "Skipping " <> show count

        findGoodCandidate (count + 1) bots

  findGoodCandidate 1 bots
