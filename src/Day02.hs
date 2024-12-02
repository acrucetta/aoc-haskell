{-# LANGUAGE DerivingStrategies #-}

module Day02 where
import Paths_aoc (getDataFileName)
import Prelude hiding (round)
import Debug.Trace
import Lib (strToIntList)

{-
Steps:
- Calculate (-) from left to right in each list
- Each one check:
  - If they're all positive or all negative. 
  - If abs difference is at least one or at most three
    i.e., filter any number at or above 4 if > 0 fail
-}

isReportValid :: [Int] -> Bool
isReportValid report =
  let diffs = zipWith (-) (tail report) report
      increasingOrDecreasing = all (>0) diffs || all (<0) diffs
      absDiffs = map abs diffs
      withinThreshold = all (>0) absDiffs && all (<=3) absDiffs
  in increasingOrDecreasing && withinThreshold

isReportValidFlex :: [Int] -> Bool
isReportValidFlex report =
  -- We need to check how many are increasing or decreasin
  -- Then check if by adding 1 to it we meet the length
  -- If so, then tag it somehow
  -- Also keep a tally of the values outside the threshold
  let diffs = zipWith (-) (tail report) report
      increasingCount = length (filter (>0) diffs)
      decreasingCount = length (filter (<0) diffs)
      zerosCount = length $ filter (==0) diffs
      absDiffs = map abs diffs
      thresholdCount = length $ filter (\x -> x == 0 || x > 3) absDiffs
  in trace ("Increasing count: " ++ show increasingCount ++
            ", Decreasing count: " ++ show decreasingCount ++
            ", Threshold count: " ++ show thresholdCount)
     (increasingCount + thresholdCount) <= 1 || (decreasingCount + thresholdCount) <= 1

solve1 :: [Char] -> Int
solve1 input =
  let reports = map (strToIntList . words) (lines input)
      validReports = filter id $ map isReportValid reports
      totalValid = length validReports
  in totalValid

solve2 :: [Char] -> Int
solve2 input =
  let reports = map (strToIntList . words) (lines input)
      validReports = filter id $ map isReportValidFlex reports
      totalValid = length validReports
  in totalValid

day02 :: IO ()
day02 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
