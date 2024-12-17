{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Day17 where

import AOC
import Control.Monad (guard)
import Data.Array.IArray ((!), elems, listArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (xor, shiftL, (.|.))
import Data.Function (applyWhen)

data Machine = Machine
  { regA :: Int
  , regB :: Int
  , regC :: Int
  , pc   :: Int
  , size :: Int
  , prog :: UArray Int Int
  }

combo :: Machine -> Int -> Int
combo m o | o < 4 = o
combo Machine{regA} 4 = regA
combo Machine{regB} 5 = regB
combo Machine{regC} 6 = regC

step :: Machine -> Machine
step m = m { pc = pc m + 2 }

exec :: Machine -> [Int]
exec m@Machine{..} | pc < size =
  let i  = prog ! pc
      op = prog ! (pc + 1) in
  applyWhen (i == 5) (combo m op `mod` 8 :) $ exec $
  applyWhen (i /= 3) step $ case i of
    0             -> m { regA = regA `div` (2 ^ combo m op)    } -- adv
    1             -> m { regB = regB `xor` op                  } -- bxl
    2             -> m { regB = combo m op `mod` 8             } -- bst
    3 | regA == 0 -> step m                                      -- jnz
    3             -> m { pc = op }                               -- jnz
    4             -> m { regB = regB `xor` regC                } -- bxc
    5             -> m                                           -- out
    6             -> m { regB = regA `div` (2 ^ combo m op)    } -- bdv
    7             -> m { regC = regA `div` (2 ^ combo m op)    } -- cdv
exec _ = []


-- reverse computation of A (specific to input)
computeA :: [Int] -> [Int]
computeA [] = [0]
computeA (x:xs) = do
  highA <- computeA xs
  lowA  <- [0..7]
  let a = highA `shiftL` 3 .|. lowA
      b = lowA `xor` 2
      c = a `div` (2 ^ b)
  guard $ x == (b `xor` c `xor` 3) `mod` 8
  return a


machine :: Int -> [Int] -> Machine
machine a ops = Machine a 0 0 0 l arr
  where l   = length ops
        arr = listArray (0, l - 1) ops

demo :: Machine
demo = machine 729 [0,1,5,4,3,0]

input :: Machine
input = machine 35200350 [2,4,1,2,7,5,4,7,1,3,5,5,0,3,3,0]


main :: IO ()
main = do
  print $ exec demo
  print $ exec input
  print $ sort $ computeA $ elems $ prog input
