{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05 where

import Data.IntSet (IntSet, fromList, isSubsetOf)
import Data.List.Split (splitOn)
import Data.Map (valid)
import Debug.Trace (trace)
import Paths_aoc (getDataFileName)
import Lib (strToIntList)
import GHC.Conc (par)

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
validatePage :: Int -> [Int] -> [(Int, Int)] -> Bool
validatePage currUpdate restUpdates rules =
  let validPages = nextPages currUpdate rules
      remainingPages = fromList restUpdates
   in remainingPages `isSubsetOf` validPages

-- Check if based on a list of pages, the update is valid
validateUpdate :: [Int] -> [(Int, Int)] -> Bool
validateUpdate [] _ = True
validateUpdate [x] _ = True
validateUpdate (x : xs) rules = validatePage x xs rules && validateUpdate xs rules

getMiddle:: [Int] -> Int
getMiddle lst = lst !! (length lst `div` 2)

getMiddlePages :: [(Bool,[Int])] -> [Int] -> [Int]
getMiddlePages [] acc = acc
getMiddlePages [x] acc = if fst x then acc ++ [getMiddle (snd x)] else acc
getMiddlePages (x:xs) acc = if fst x then getMiddlePages xs (acc ++ [getMiddle (snd x)]) else getMiddlePages xs acc

solve1 :: [Char] -> Int
solve1 input =
  let parts = splitOn "\n\n" input
      (rules, updates) = (buildRuleset $ words $ head parts, words $ last parts)
      parsedUpdates = map (strToIntList . splitOn ",") updates
      validUpdates = map (\update -> validateUpdate update rules) parsedUpdates
      updatesWithFlag = zip validUpdates parsedUpdates
      middlePages = getMiddlePages updatesWithFlag []
   in trace (show middlePages) sum middlePages

solve2 :: [Char] -> Int
solve2 input = do
  23

day05 :: IO ()
day05 = do
  input <- getDataFileName "day05-input.txt" >>= readFile
  print $ solve1 input

-- print $ solve2 input
