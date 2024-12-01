module Day01 where

import Paths_aoc2024 (getDataFileName)
import Data.List.Split (splitOn)
import Data.List (sort, transpose, nub)
import Debug.Trace
import Data.Char

{-
Steps:
- Parse each line
- Split by the space
- Put the first column into a list, the second column into another list
- Sort and subtract index by index
-}

readInts :: [[String]] -> [[Int]]
readInts = map (map read)

absDiff :: Num a => a -> a -> a
absDiff = \x y -> abs (x-y)

solve1 :: [Char] -> Int
solve1 input = do
  let groups = transpose $ readInts $ map words (lines input)
  let (g1, g2) = (sort $ head groups, sort $ last groups)
  let totalDistance = sum $ zipWith absDiff g1 g2
  trace(show totalDistance) (totalDistance)

{-
Steps:
- Parse each line
- Split by the space
- Put the first column into a list, the second column into another list
- Count unique numbers in g1, then check how many times they exist in g2; get result as counts
- Add all the counts
-}

-- Count number of times value N occurs in list Xs
similarityScore :: Int -> [Int] -> Int
similarityScore num xs = trace (show result) result
  where result = num * (length $ filter (==num) xs)

solve2 :: [Char] -> Int
solve2 input = do
  let groups = transpose $ readInts $ map words (lines input)
  let (g1, g2) = (head groups, last groups)
  let similarCounts = map (\x -> similarityScore x g2) g1
  let similarityTotal = sum similarCounts
  trace(show similarityTotal) (similarityTotal)

day01 :: IO ()
day01 = do
  input <- getDataFileName "day01-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
