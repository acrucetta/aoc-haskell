module DayXY where

import Paths_aoc2023 (getDataFileName)

solve1 :: [Char] -> Int
solve1 input = do
  23

solve2 :: [Char] -> Int
solve2 input = do
  23

dayXY :: IO ()
dayXY = do
  input <- getDataFileName "dayXX-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
