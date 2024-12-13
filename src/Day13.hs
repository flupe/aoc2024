{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day13 where

import AOC

type Coord   = (Int, Int)
data Machine = Machine Coord Coord Coord

machineP :: Parser Machine
machineP = Machine
  <$> ((,) <$ "Button A: X+" <*> decimal <* ", Y+" <*> decimal <* "\n")
  <*> ((,) <$ "Button B: X+" <*> decimal <* ", Y+" <*> decimal <* "\n")
  <*> ((,) <$ "Prize: X="    <*> decimal <* ", Y=" <*> decimal)

cross :: Coord -> Coord -> Int
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

score :: Machine -> Maybe Int
score (Machine a@(ax, ay) b@(bx, by) p@(px, py)) =
  (3 * x + y) <$ guard (xr == 0 && yr == 0)
  where d       = cross a b
        (y, yr) = cross a p `divMod` d
        (x, xr) = cross p b `divMod` d

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
