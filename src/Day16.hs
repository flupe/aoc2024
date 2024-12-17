{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, BlockArguments, LambdaCase, ViewPatterns #-}

module Day16 where

import AOC
import Control.Monad (foldM, filterM, forM)
import Control.Monad.ST (ST)
import Data.Array.IArray as IArr
import Data.Array.MArray as MArr
import Data.Array
import Data.Array.Unboxed (UArray)
import Data.Array.ST
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.List (transpose, find, sortOn, group, nub, groupBy)
import Data.Set qualified as Set
import Data.Text qualified as Text

type Coord = (Int, Int)
data Dir = U | R | D | L deriving (Eq, Ord, Enum, Show, Bounded)

-- courtesy of sára juhošová
-- (im a bad programmer)
findPaths :: Coord -> Coord -> UArray Coord Char -> Maybe (Int, [[Coord]])
findPaths start end grid = runST do
  let bs = IArr.bounds grid

  dists :: STArray s Coord [(Dir, Int)      ] <- MArr.newArray bs []
  paths :: STArray s Coord [(Dir, [[Coord]])] <- MArr.newArray bs []

  writeArray dists start [(R, 0)]
  writeArray paths start [(R, [[start]])]

  let
    aux :: Set (Int, Coord, Dir)
        -> ST s ()
    aux queue | Nothing                         <- Set.minView queue = pure ()
    aux queue | Just ((pScore, p, pdir), queue) <- Set.minView queue = do
      pPaths <- fromMaybe [] . lookup pdir <$> readArray paths p

      let ns = filter (('#' /= ) . (grid IArr.!) . snd) $ neighbours p

      -- only keep neighbours we've made progress with
      ns' <- catMaybes <$> forM ns \(ndir, n) -> do
        let nScore' = pScore + (if pdir == ndir then 1 else 1001)
        nScore <- fromMaybe maxBound . lookup ndir <$> readArray dists n
        let nPaths = (n:) <$> pPaths -- new paths to n through p
        if nScore < nScore' then pure Nothing
        else if nScore == nScore' then do
          modifyArray' paths n $ updateAssocWith (++) ndir nPaths
          pure Nothing
        else do
          modifyArray' dists n $ insertAssoc ndir nScore'
          modifyArray' paths n $ insertAssoc ndir nPaths
          pure $ Just (nScore', n, ndir)

      aux $ foldr Set.insert queue ns'

  aux (Set.singleton (0, start, R))

  pathsToEnd <- sortOn fst . catMaybes <$>
    forM [U, R, D, L] \dir -> do
      dist  <- lookup dir <$> readArray dists end
      paths <- lookup dir <$> readArray paths end
      pure ((,) <$> dist <*> paths)

  case pathsToEnd of
    [] -> pure Nothing
    ((dist, ps):rest) -> pure $
      Just (dist, ps ++ concat (snd <$> takeWhile ((dist==) . fst) rest))


neighbours :: Coord -> [(Dir, Coord)]
neighbours (x, y) =
  [ (L, (x - 1, y))
  , (R, (x + 1, y))
  , (U, (x, y - 1))
  , (D, (x, y + 1))
  ]

findCoord :: Char -> UArray Coord Char -> Maybe Coord
findCoord c grid = find ((c==) . (grid IArr.!)) $ range (IArr.bounds grid)

main :: IO ()
main = do
  src <- readFile "inputs/16" <&> strip <&> lines <&> map unpack

  let width  = length (src !! 0)
      height = length src
      bounds = ((0, 0), (width - 1, height - 1))
      grid   = IArr.listArray bounds $ concat $ transpose src

  let Just start = findCoord 'S' grid
  let Just end   = findCoord 'E' grid

  let Just (dist, paths) = findPaths start end grid

  print dist
  print $ length $ nub $ concat paths
