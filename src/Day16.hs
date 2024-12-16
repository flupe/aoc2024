{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, LambdaCase, ViewPatterns #-}

module Day16 where

import AOC
import Control.Monad (foldM, filterM)
import Control.Monad.ST (ST)
import Data.Array.IArray as IArr
import Data.Array.MArray as MArr
import Data.Array
import Data.Array.Unboxed (UArray)
import Data.Array.ST
import Data.Set (Set)
import Data.List (transpose, find, sortOn, group, nub, groupBy)
import Data.Set qualified as Set
import Data.Text qualified as Text

for = flip map

type Coord = (Int, Int)
type Queue = Set (Int, Coord)

-- dijkstra algorithm adapted to a 2d grid
-- data.set used as priority queue
-- TODO: early cutoff?
dijkstra 
  :: Coord -> Coord -> UArray Coord Char 
  -> ( UArray Coord Int
     , Array Coord [[Coord]]
     )
dijkstra source target grid = runST do
  let bs = IArr.bounds grid

  dists :: STUArray s Coord Int       <- MArr.newArray bs maxBound
  paths :: STArray  s Coord [[Coord]] <- MArr.newArray bs []

  writeArray dists source 0
  writeArray paths source [[source]]

  let
    aux :: Queue -> ST s ()
    aux q
      | Just ((pScore, p@(x, y)), q) <- Set.minView q
      = do
        pathsToP <- readArray paths p

        let
          neighbours :: [Coord]
          neighbours =
            filter (('#' /=) . (grid IArr.!))
              [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

          scoreIncrement :: Coord -> [Coord] -> Int
          scoreIncrement x [y    ] | snd x     /= snd y = 1001
          scoreIncrement x [y    ] | fst x + 1 == fst y = 2001
          scoreIncrement x (y:z:_) | inDiag x z         = 1001
          scoreIncrement x _                            = 1

          extendPathsToP :: Coord -> (Int, [[Coord]])
          extendPathsToP p' =
            let pps = for pathsToP (\path -> (scoreIncrement p' path, p' : path))
                        & sortOn fst
                        & groupBy (\x y -> fst x == fst y)
                        & head
            in (fst $ pps !! 0, map snd pps)

          add :: Queue -> Coord -> ST s Queue
          add queue p' = do
            let (dt, pathsToP'FromP) = extendPathsToP p'
            let scoreToP'FromP = pScore + dt
            oldScore <- readArray dists p'
            if oldScore < scoreToP'FromP then pure queue
            else if oldScore == scoreToP'FromP then do
              modifyArray' paths p' (pathsToP'FromP ++)
              pure queue
            else do
              writeArray paths p' pathsToP'FromP
              writeArray dists p' scoreToP'FromP
              pure $ Set.insert (scoreToP'FromP, p')
                   $ Set.delete (oldScore, p') queue
        foldM add q neighbours >>= aux
    aux _ = pure ()

  aux (Set.singleton (0, source))

  (,) <$> freeze dists <*> freeze paths


inDiag :: Coord -> Coord -> Bool
inDiag (y1, x1) (y2, x2) = y1 /= y2 && x1 /= x2

findCoord :: Char -> UArray Coord Char -> Maybe Coord
findCoord c grid = find ((c==) . (grid IArr.!)) $ range (IArr.bounds grid)

main :: IO ()
main = do
  src <- readFile "inputs/16" <&> strip <&> lines <&> map unpack

  -- {-
  let src = ["###############",
             "#.......#....E#",
             "#.#.###.#.###.#",
             "#.....#.#...#.#",
             "#.###.#####.#.#",
             "#.#.#.......#.#",
             "#.#.#####.###.#",
             "#...........#.#",
             "###.#.#####.#.#",
             "#...#.....#.#.#",
             "#.#.#.###.#.#.#",
             "#.....#...#.#.#",
             "#.###.#.#.#.#.#",
             "#S..#.....#...#",
             "###############"]
  -- -}
  let width  = length (src !! 0)
      height = length src
      bounds = ((0, 0), (width - 1, height - 1))
      grid   = IArr.listArray bounds $ concat $ transpose src

  let Just start = findCoord 'S' grid
  let Just end   = findCoord 'E' grid

  let (dists, paths) = dijkstra start end grid
  print $ dists IArr.! end

  let pathss = paths IArr.! end
  print (length pathss)
  print (pathss)
