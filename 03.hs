{-# LANGUAGE OverloadedStrings #-}
import AOC

main :: IO ()
main = do
  let pairP     = (*) <$ "(" <*> int <* "," <*> int <* ")"
  let runMuls s = splitOn "mul" s & mapMaybe (run pairP) & sum

  readFile "inputs/3" <&> runMuls >>= print

  readFile "inputs/3"
    <&> splitOn "do()"
    <&> mapMaybe (fmap runMuls . listToMaybe . splitOn "don't()")
    <&> sum
    >>= print
