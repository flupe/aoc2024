import AOC

type Report = [Int]

safe1 :: Report -> Bool
safe1 r =
  let diff  = zipWith (-) (tail r) r
      range = flip all (map abs diff) $ \v -> v > 0 && v < 4
      incr  = all (> 0) diff
      decr  = all (< 0) diff
   in range && (incr || decr)

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
