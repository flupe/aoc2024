{-# LANGUAGE NoImplicitPrelude, BlockArguments #-}

module Day04 where

import AOC hiding ((!))

import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (bimap)

type Shirt     = (Int, Int) -> Char
type Counter s = STRef s Int

transpose, mirror :: Shirt -> Shirt
transpose s (x, y) = s (      y, x)
mirror    s (x, y) = s (141 - x, y)

counted :: (forall s. Counter s -> ST s a) -> Int
counted f = runST do c <- newSTRef 0; f c *> readSTRef c

increment :: Counter s -> ST s ()
increment = flip modifySTRef' (+ 1) -- should be strict!!!

range :: Monad m => [Int] -> [Int] -> ((Int, Int) -> m ()) -> m ()
range rx ry = forM_ ((,) <$> rx <*> ry)

-- | @checkXMAS c sh (x, y) (dx, dy)@
--   will check if the 4-letter word in @sh@
--   * starting from @(x, y)@
--   * in direction @(dx, dy)@
--   * is either @"XMAS"@ or @"SAMX"@
--   If so, will increment counter @c@
--   Warning: no bound check is performed.
checkXMAS :: Counter s -> Shirt -> (Int, Int) -> (Int, Int) -> ST s ()
checkXMAS c sh p (dx, dy) =
  let str = sh <$> take 4 (iterate (bimap (+ dx) (+ dy)) p)
  in when (str `elem` ["XMAS", "SAMX"]) $ increment c

countXMAS :: Shirt -> Int
countXMAS sh = counted \counter ->
  range [1 .. 137] [1 .. 140] \p@(_, y) -> do
    checkXMAS counter sh             p (1, 0) -- horizontal
    checkXMAS counter (transpose sh) p (1, 0) -- vertical
    when (y > 3) do
      checkXMAS counter sh          p (1, -1) -- diagonal up
      checkXMAS counter (mirror sh) p (1, -1) -- diagonal down

countCrosses :: Shirt -> Int
countCrosses shirt = counted \counter ->
  range [2 .. 139] [2 .. 139] \k ->
    when (shirt k == 'A' && all (`elem` ["MS", "SM"]) (diags k)) $
      increment counter
  where diags (x, y) = [ [ shirt (x - 1, y - 1) , shirt (x + 1, y + 1) ]
                       , [ shirt (x - 1, y + 1) , shirt (x + 1, y - 1) ]
                       ]

main :: IO ()
main = do
  shirt :: Shirt
    <- readFile "inputs/4"
       <&> lines
       <&> concatMap unpack
       <&> listArray @UArray ((1, 1), (140, 140))
       <&> (!)

  print $ countXMAS shirt
  print $ countCrosses shirt
