{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05 where

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Paths_aoc (getDataFileName)
import Data.IntSet (fromList, IntSet)

{-
Steps:
- Split the input into the map and the rows of input
- Make a tuple set of the pairs
- For each cell in the rows of input, check that all
other cells after it come after it
e.g.. 75 -> 75|47, 75|61, 75|53, and 75|29

- Now we will iterate over each cell in the input
e.g., 75, we will get the next pages, and we will
check if the "rest" list is contained in the nextPages
  if not, we return False, else, we keep iterating
- If we're at the last item, we return True as well

TIP: If we use sets we can just determine if the rest of pages are a subset of
  the next pages in the rulebook, if so, we're good

- We build an array of Bools and return those values
-}

toTuple :: [String] -> (Int, Int)
toTuple [a, b] = (read a :: Int, read b :: Int)

buildRuleset :: [String] -> [(Int, Int)]
buildRuleset rules =
  let splitRules = map (toTuple . splitOn "|") rules
   in trace (show splitRules) splitRules

nextPages :: Int -> [(Int, Int)] -> IntSet
nextPages page rules =
  let pages = map snd $ filter (\rule -> page == fst rule) rules
   in fromList pages

-- Check if based on the current page, the next updates are valid
validatePage :: Int -> [Int] -> Bool
validatePage currUpdate restUpdates = 
  True

-- Check if based on a list of pages, the update is valid
validateUpdate :: [Int] -> Bool
validateUpdate updates = True

solve1 :: [Char] -> Int
solve1 input =
  let parts = splitOn "\n\n" input
      (rules, updates) = (words $ head parts, words $ last parts)
      ruleSet = buildRuleset rules
      samplePages = nextPages 47 ruleSet
   in trace (show samplePages) 23

solve2 :: [Char] -> Int
solve2 input = do
  23

day05 :: IO ()
day05 = do
  input <- getDataFileName "day05-input.txt" >>= readFile
  print $ solve1 input

-- print $ solve2 input
