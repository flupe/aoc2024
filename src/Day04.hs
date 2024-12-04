{-# LANGUAGE BlockArguments #-}

module Day04 where

import Prelude hiding (readFile, lines)
import Data.Functor ((<&>))
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef)
import Data.Text (unpack, lines)
import Data.Text qualified as Text (unpack)
import Data.Text.IO (readFile)
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_, when)

type Shirt = (Int, Int) -> Char

transpose :: Shirt -> Shirt
transpose s (x, y)  = s (y, x)

mirror :: Shirt -> Shirt
mirror s (x, y) = s (141 - x, y)

counted :: (forall s. STRef s Int -> ST s ()) -> Int
counted f = runST do
  counter <- newSTRef 0
  f counter
  readSTRef counter

increment :: STRef s Int -> ST s ()
increment = flip modifySTRef (+1)

findHoriz :: Shirt -> Int
findHoriz shirt = counted \counter ->
  forM_ ((,) <$> [1 .. 137] <*> [1 .. 140]) $ \(x, y) ->
    when ((shirt <$> [ (x + o, y) | o <- [0 .. 3] ]) `elem` ["XMAS", "SAMX"]) $
      increment counter

findDiag :: Shirt -> Int
findDiag shirt = counted \counter -> do
  forM_ [4 .. 140] \y ->
    forM_ [1 .. 137] \x ->
      when ((shirt <$> [ (x + o, y - o) | o <- [0 .. 3] ]) `elem` ["XMAS", "SAMX"]) $
        increment counter

findXMAS :: Shirt -> Int
findXMAS shirt = sum $ ops <*> [shirt]
  where ops = [ findHoriz
              , findHoriz . transpose
              , findDiag
              , findDiag . mirror
              ]

findCrosses :: Shirt -> Int
findCrosses shirt = counted \counter ->
  forM_ ((,) <$> [2 .. 139] <*> [2 .. 139]) $ \k ->
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
       <&> concatMap (Text.unpack)
       <&> listArray @UArray ((1, 1), (140, 140))
       <&> (!)

  print $ findXMAS shirt
  print $ findCrosses shirt
