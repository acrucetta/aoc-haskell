{-# LANGUAGE DerivingStrategies #-}

module Day02 where

import Debug.Trace
import Lib (toIntList)
import Paths_aoc (getDataFileName)
import Prelude hiding (round)

{-
Steps:
- Calculate (-) from left to right in each list
- Each one check:
  - If they're all positive or all negative.
  - If abs difference is at least one or at most three
    i.e., filter any number at or above 4 if > 0 fail
-}

isSafe :: [Int] -> Bool
isSafe report =
  let diffs = zipWith (-) (tail report) report
      increasingOrDecreasing = all (> 0) diffs || all (< 0) diffs
      absDiffs = map abs diffs
      withinThreshold = all (> 0) absDiffs && all (<= 3) absDiffs
   in increasingOrDecreasing && withinThreshold

anySafe :: [Int] -> Bool
anySafe report =
  any (\i -> isSafe (take i report ++ drop (i + 1) report)) [0 .. length report - 1]

solve1 :: [Char] -> Int
solve1 input =
  let reports = map (toIntList . words) (lines input)
      validReports = filter id $ map isSafe reports
      totalValid = length validReports
   in totalValid

solve2 :: [Char] -> Int
solve2 input =
  let reports = map (toIntList . words) (lines input)
      -- ([any([is_safe(row[:i] + row[i + 1:]) for i in range(len(row))])
      validReports = filter id $ map anySafe reports
      totalValid = length validReports
   in totalValid

day02 :: IO ()
day02 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
