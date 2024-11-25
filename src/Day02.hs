module Day02 where

import Paths_aoc2023 (getDataFileName)

solve1 :: [Char] -> Int
solve1 input = do
  23

solve2 :: [Char] -> Int
solve2 input = do
  23


day02 :: IO ()
day02 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
