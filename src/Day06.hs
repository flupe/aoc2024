{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ViewPatterns, LambdaCase, BlockArguments #-}

module Day06 where

import AOC
import Data.Array.IO
import Data.Array.MArray
import Data.Ix
import Control.Applicative (some, (<|>))
import Control.Monad (guard, forM, filterM)
import Data.Functor (($>))
import Data.Maybe (catMaybes)

data Dir  = U | R | D | L deriving (Eq, Show)
type Pos  = (Int, Int)
data Cell = Wall | Empty | Visited [Dir] deriving (Eq)
data Grid = Grid { size :: Int, grid :: IOArray (Int, Int) Cell }

turn :: Dir -> Dir
turn U = R
turn R = D
turn D = L
turn L = U

data St = St { pos :: Pos, dir :: Dir } deriving Show

step :: Dir -> Pos -> Pos
step U (y, x) = (y - 1, x    )
step R (y, x) = (y    , x + 1)
step D (y, x) = (y + 1, x    )
step L (y, x) = (y    , x - 1)

cellP :: Parser Cell
cellP = Wall <$ "#" <|> Empty <$ "." <|> Visited [U] <$ "^"

move :: Grid -> St -> IO (Either St Bool)
move g@(Grid size grid) st@(St pos dir) =
  let p' = step dir pos in
  if not $ inRange ((1, 1), (size, size)) p' then return $ Right False
  else do 
    next <- readArray grid p'
    case next of
      Wall                -> move g st { dir = turn dir }
      Visited dirs 
        | dir `elem` dirs -> pure $ Right True
        | otherwise       -> writeArray grid p' (Visited $ dir : dirs) $> Left st { pos = p' }
      Empty               -> writeArray grid p' (Visited [dir])        $> Left st { pos = p' }

loops :: Grid -> St -> IO Bool
loops gd st = move gd st >>= \case
  Right b  -> pure b
  Left st' -> loops gd st'

main :: IO ()
main = do
  input :: [Text] <- readFile "inputs/6" <&> lines

  -- let input :: [Text] = ["....#.....",
  --                        ".........#",
  --                        "..........",
  --                        "..#.......",
  --                        ".......#..",
  --                        "..........",
  --                        ".#..^.....",
  --                        "........#.",
  --                        "#.........",
  --                        "......#..."]

  let size = length input
  let copyArray :: IOArray Pos Cell -> IO (IOArray Pos Cell) = mapArray id

  arr :: IOArray Pos Cell
    <- input
       & mapMaybe (run $ some cellP)
       & concat
       & newListArray @IOArray ((1, 1), (size, size))

  [fst -> start] <- getAssocs arr <&> filter ((== Visited [U]) . snd) <&> take 1

  let grid  = Grid size arr
  let state = St start U

  grids :: [Grid] <- catMaybes <$> forM (range ((1, 1), (size, size))) \p -> do
    cell <- readArray arr p
    if cell /= Empty
      then pure Nothing
      else do arr' <- copyArray arr
              writeArray arr' p Wall
              pure $ Just (Grid size arr')

  loopings <- filterM (flip loops state) grids
  print $ length loopings

  -- getElems arr 
  --   <&> filter (not . (`elem` [Empty, Wall]))
  --   <&> length
  --   >>= print
