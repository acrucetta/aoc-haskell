module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOn)
import Data.List (sort)

solve1 :: [Char] -> Int
solve1 input = do
  let elfPacks = map lines $ splitOn "\n\n" input
  let packDigits = map (map (read :: String -> Int)) elfPacks
  let packSums = map sum packDigits
  maximum packSums

solve2 :: [Char] -> Int
solve2 input = do
  let elfPacks = map lines $ splitOn "\n\n" input
  let packDigits = map (map (read :: String -> Int)) elfPacks
  let packSums = map sum packDigits
  let top3sums = sum $ take 3 $ reverse $ sort packSums
  top3sums


day01 :: IO ()
day01 = do
  input <- getDataFileName "day01-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
