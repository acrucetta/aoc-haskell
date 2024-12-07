{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day07 where

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Lib (toIntList)
import Paths_aoc (getDataFileName)

{-
Steps:

Goal: Figure out if with add and multiply the operators will meet our target number
We want to classify as True the equations that meet the result and add them.

Steps:
- Parse the input into targer (int) and numbers [Int] -> (Int,[Int])
- Figure out how many operations we could use (n-1) where n = length of numbers
- Do some form of fold left with each operator in order,
figure out how to do this
- For each operator result, check how much we got and if it matches the target
  return True
-}

solve1 :: [String] -> Int
solve1 input =
  let rows = map (splitOn ":") input
      equations = [(read $ head row :: Int, (toIntList . words) $ last row) | row <- rows]
   in trace (show equations) 23

solve2 :: [String] -> Int
solve2 input = do
  23

day07 :: IO ()
day07 = do
  input <- getDataFileName "day07-input.txt" >>= readFile
  print $ solve1 $ lines input
  print $ solve2 $ lines input
