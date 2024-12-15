{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase, BlockArguments, RecordWildCards, ViewPatterns #-}

module Day15 where

import AOC
import Data.Text (breakOn)
import Data.Ix
import Data.Array.ST
import Data.Array.MArray as MA
import Data.Array.IArray as IA
import Data.List (find, transpose)


data Dir    = U | R | D | L deriving (Eq, Show)
data Cell   = Wall | Box | BoxL | BoxR | Empty deriving (Eq, Show)
type Coord  = (Int, Int)
data Grid s = Grid { bs :: (Coord, Coord), arr :: STArray s Coord Cell }

toDir :: Char -> Maybe Dir
toDir c = lookup c [('^', U), ('>', R), ('v', D), ('<', L)]

shift :: Dir -> Coord -> Coord
shift U (x, y) = (x    , y - 1)
shift R (x, y) = (x + 1, y    )
shift D (x, y) = (x    , y + 1)
shift L (x, y) = (x - 1, y    )

toCell :: Char -> Cell
toCell '#'  = Wall
toCell 'O'  = Box
toCell '['  = BoxL
toCell ']'  = BoxR
toCell _    = Empty


canMove :: Grid s -> Dir -> Coord -> ST s Bool
canMove g@Grid{bs} d p | not (inRange bs p) = pure False
canMove g@Grid{..} d p =
  readArray arr p >>= \case
    Empty -> pure True
    Wall  -> pure False
    Box   -> canMove g d $ shift d p
    BoxL  ->
      -- a left box can only move up or down if its right counterpart can
      let next = shift d p in
      if (d == U || d == D) 
        then (&&) <$> canMove g d next <*> canMove g d (shift R next)
        else canMove g d next
    BoxR  ->
      -- a right box can only move up or down if its left counterpart can
      let next = shift d p in
      if (d == U || d == D) 
        then (&&) <$> canMove g d next <*> canMove g d (shift L next)
        else canMove g d next


-- we assume canMove
move :: Grid s -> Dir -> Coord -> ST s ()
move g@Grid{bs} d p | not (inRange bs p) = pure ()
move g@Grid{..} d p =
  readArray arr p >>= \case
    Empty -> pure ()
    Wall  -> pure ()
    Box -> do
      let next = shift d p
      move g d next
      writeArray arr next Box
      writeArray arr p Empty
    BoxL -> do
      let next = shift d p
      move g d next
      writeArray arr next BoxL
      writeArray arr p    Empty
      -- when moving up or down, a left box brings along its right counterpart
      when (d == U || d == D) $ move g d $ shift R p
    BoxR -> do
      let next = shift d p
      move g d next
      writeArray arr next BoxR
      writeArray arr p    Empty
      -- when moving up or down, a right box brings along its left counterpart
      when (d == U || d == D) $ move g d $ shift L p


steps :: Grid s -> Coord -> [Dir] -> ST s ()
steps g p [] = pure ()
steps g@Grid{..} p (d:ds) = do
  let next = shift d p
  canMove <- canMove g d next
  p' <- if canMove then next <$ move g d next else pure p
  steps g p' ds


preprocess :: [[Char]] -> ((Coord, Coord), Array Coord Char)
preprocess xs =
  let width  = length (xs !! 0)
      height = length xs
      bounds = ((0, 0), (width - 1, height - 1))
  in (bounds, listArray bounds $ concat $ transpose xs)


part :: [Dir] -> Cell -> [[Char]] -> Int
part ds t xs = runST do
  let (bounds, g) = preprocess xs

  -- setup grid and find start position
  let Just start = find ((=='@') . (g IA.!)) $ range bounds
  g' :: Grid s <- Grid bounds <$> (MA.mapArray toCell =<< thaw g)

  -- move boxes around
  steps g' start ds

  -- sum up gps coord of all cells equal to target t
  getAssocs (arr g')
    <&> filter ((== t) . snd)
    <&> map (\((x, y), _) -> 100 * y + x)
    <&> sum


widen :: String -> String
widen = concatMap \case
  '#' -> "##"
  'O' -> "[]"
  '.' -> ".."
  '@' -> "@."
  

main :: IO ()
main = do
  (tgrid, tdirs) <- readFile "inputs/15" <&> strip <&> breakOn "\n\n"

  let dirs = mapMaybe toDir $ unpack $ mconcat $ lines tdirs
      grid = map unpack $ lines tgrid

  print $ part dirs Box  $ grid
  print $ part dirs BoxL $ map widen grid
