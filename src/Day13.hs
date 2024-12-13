{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day13 where

import AOC
import Data.Text (strip)
import Control.Monad (guard)

type Coord = (Int, Int)
data Machine = Machine { a :: Coord, b :: Coord, prize :: Coord} deriving Show

machineP :: Parser Machine
machineP = Machine
  <$> ((,) <$ "Button A: X+" <*> decimal <* ", Y+" <*> decimal <* "\n")
  <*> ((,) <$ "Button B: X+" <*> decimal <* ", Y+" <*> decimal <* "\n")
  <*> ((,) <$ "Prize: X="    <*> decimal <* ", Y=" <*> decimal)

score :: Machine -> Maybe Int
score (Machine (ax, ay) (bx, by) (px, py)) = do
  let y1 = ax * py - ay * px
      y2 = ax * by - ay * bx
      x1 = by * px - bx * py
      x2 = by * ax - bx * ay
      y  = y1 `div` y2
      x  = x1 `div` x2
  guard (x1 `rem` x2 == 0 && y1 `rem` y2 == 0)
  pure (3 * x + y)

main :: IO ()
main = do
  machines <- readFile "inputs/13"
                <&> strip
                <&> splitOn "\n\n"
                <&> mapMaybe (run machineP)

  let correct :: Machine -> Machine
      correct (Machine a b (px, py)) = Machine a b (px + o, py + o)
        where o = 10000000000000

      tokens = sum . mapMaybe score

  print $ tokens machines
  print $ tokens $ map correct machines
