import AOC

main :: IO ()
main = do
  input :: [[Int]]
    <- readFile "inputs/1"
       <&> lines
       <&> map (map read . splitOn "   ")

  let xs = sort $ map (!! 0) input
  let ys = sort $ map (!! 1) input

  let count x = length $ filter (x ==) ys

  print $ sum $ zipWith (abs .: (-)) xs ys
  print $ sum $ map ((*) <*> count) xs
