{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05 where

import Data.IntSet (IntSet, fromList, isSubsetOf)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Paths_aoc (getDataFileName)
import Lib (toIntList)
import Data.Graph (buildG, topSort)

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
isUpdateValid :: [Int] -> [(Int, Int)] -> Bool
isUpdateValid [] _ = True
isUpdateValid [x] _ = True
isUpdateValid (x : xs) rules = validatePage x xs rules && isUpdateValid xs rules

getMiddle:: [Int] -> Int
getMiddle lst = lst !! (length lst `div` 2)

getMiddlePages :: [[Int]] -> [Int] -> [Int]
getMiddlePages [] acc = acc
getMiddlePages [x] acc = acc ++ [getMiddle x]
getMiddlePages (x:xs) acc = getMiddlePages xs (acc ++ [getMiddle x])

solve1 :: [Char] -> Int
solve1 input =
  let parts = splitOn "\n\n" input
      (rules, updates) = (buildRuleset $ words $ head parts, words $ last parts)
      parsedUpdates = map (toIntList . splitOn ",") updates
      flagList = map (\update -> isUpdateValid update rules) parsedUpdates
      updatesWithFlag = zip flagList parsedUpdates
      validUpdates = map snd $ filter fst updatesWithFlag
      middlePages = getMiddlePages validUpdates []
   in trace (show middlePages) sum middlePages


validateUpdate :: [(Int, Int)] -> [Int] -> [Int]
validateUpdate rules update =
  let 
      filteredRules = filter (\(c1, c2) -> c1 `elem` update && c2 `elem` update) rules
      -- Create edges for the graph from filtered rules
      edges = [(c1, c2) | (c1, c2) <- filteredRules]
      -- Find min and max nodes to define graph bounds
      nodes = concatMap (\(a, b) -> [a, b]) filteredRules
      minNode = minimum nodes
      maxNode = maximum nodes
      graph = buildG (minNode, maxNode) edges
      sortedUpdate = topSort graph
      -- Filter sorted nodes to only include those from the original update
      -- (since graph might include extra nodes from the rules)
      finalUpdate = filter (`elem` update) sortedUpdate
   in finalUpdate

solve2 :: [Char] -> Int
solve2 input = do
  let parts = splitOn "\n\n" input
      (rules, updates) = (buildRuleset $ words $ head parts, words $ last parts)
      parsedUpdates = map (toIntList . splitOn ",") updates
      flagList = map (`isUpdateValid` rules) parsedUpdates
      updatesWithFlag = zip flagList parsedUpdates
      invalidUpdates = map snd $ filter (not . fst) updatesWithFlag
      validatedUpdates = map (validateUpdate rules) invalidUpdates
      validatedMiddlePages = getMiddlePages validatedUpdates []
   in sum validatedMiddlePages

day05 :: IO ()
day05 = do
  input <- getDataFileName "day05-input.txt" >>= readFile
  -- print $ solve1 input
  print $ solve2 input
