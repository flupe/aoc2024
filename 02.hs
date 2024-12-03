{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import AOC

type Report = [Int]

safe1 :: Report -> Bool
safe1 r = range && (incr || decr)
  where diff  = zipWith (-) (drop 1 r) r
        range = all ((< 4) . abs) diff
        incr  = all (> 0) diff
        decr  = all (< 0) diff

subs :: Report -> [Report]
subs [] = []
subs (x:xs) = xs : ((x :) <$> subs xs)

safe2 :: Report -> Bool
safe2 r = any safe1 (r : subs r)

main :: IO ()
main = do
  reports :: [Report]
    <- readFile "inputs/2"
       <&> lines
       <&> map (map read . splitOn " ")

  print $ length $ filter safe1 reports
  print $ length $ filter safe2 reports
