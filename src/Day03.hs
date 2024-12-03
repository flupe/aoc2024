{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day03 where

import AOC

main :: IO ()
main = do
  let pairP     = (*) <$ "(" <*> decimal <* "," <*> decimal <* ")"
      runMuls s = splitOn "mul" s & mapMaybe (run pairP) & sum

  readFile "inputs/3" <&> runMuls >>= print

  readFile "inputs/3"
    <&> splitOn "do()"
    <&> mapMaybe (fmap runMuls . listToMaybe . splitOn "don't()")
    <&> sum
    >>= print
