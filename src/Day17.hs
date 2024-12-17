{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, BlockArguments #-}

module Day17 where

import AOC hiding (splitAt)
import Data.Functor (($>))
import Data.Text (breakOn)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Text.Megaparsec.Char (asciiChar)
import Control.Applicative.Combinators
import Data.Bits
import Debug.Trace (trace)

data Machine = Machine
  { regA :: Int
  , regB :: Int
  , regC :: Int
  , pc   :: Int
  , size :: Int
  , prog :: UArray Int Int
  }

toArray :: [Int] -> UArray Int Int
toArray xs = listArray (0, length xs - 1) xs


demo :: Machine
demo = Machine 2024 0 0 0 6 $ toArray [0, 3, 5, 4, 3, 0]

input :: Machine
input = Machine 35200350 0 0 0 15 $ toArray [2, 4, 1, 2, 7, 5, 4, 7, 1, 3, 5, 5, 0, 3, 3, 0]

-- reverse computation of A (specific to my input prog)
computeA :: [Int] -> [Int]
computeA [] = [0]
computeA (x:xs) = do
  highA <- flip shiftL 3 <$> computeA xs 
  lowA <- flip filter [0 .. 7] \a ->
     let b = a `xor` 2
         c = (highA .|. a) `div` (2 ^ b)
     in x == (((b `xor` c) `xor` 3) `mod` 8)
  pure (highA .|. lowA)


combo :: Machine -> Int -> Int
combo m o | o < 4 = o
combo Machine{regA} 4 = regA
combo Machine{regB} 5 = regB
combo Machine{regC} 6 = regC
combo _ 7 = undefined

step :: Machine -> Machine
step m = m { pc = pc m + 2 }

jump :: Int -> Machine -> Machine
jump i m = m { pc = i }

exec :: Machine -> [Int]
exec m@(Machine{..}) | pc < size =
  let op = prog ! (pc + 1) in
  case prog ! pc of
    0 -> exec $ step $ m { regA = regA `div` (2 ^ combo m op) } -- adv
    1 -> exec $ step $ m { regB = regB `xor` op               } -- bxl
    2 -> exec $ step $ m { regB = combo m op `mod` 8          } -- bst
    3 -> if regA == 0 then exec $ step m                        -- jnz
                      else exec $ jump op m
    4 -> exec $ step $ m { regB = regB `xor` regC             } -- bxc
    5 -> (combo m op `mod` 8 :) $ exec $ step m                 -- out
    6 -> exec $ step $ m { regB = regA `div` (2 ^ combo m op) } -- bdv
    7 -> exec $ step $ m { regC = regA `div` (2 ^ combo m op) } -- cdv
exec m@(Machine{..}) = []

main :: IO ()
main = do
  -- part1
  print $ exec input

  -- part2
  print $ head $ sort $ computeA $ elems $ prog input
