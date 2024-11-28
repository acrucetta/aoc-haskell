module Day03 where

import Paths_aoc2023 (getDataFileName)
import Data.List (intersect)
import Data.Char
import Debug.Trace (trace)

-- Steps:
-- 1. Split into two compartments
-- 2. Find letters in common among the two
-- 3. Calculate the letter value for each one
-- 4. Sum all the letters

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

strToList :: [a] -> [[a]]
strToList s = map (\c -> [c]) s

charToScore :: Char -> Int
charToScore c
 | isAsciiLower c = ord c - ord 'a' + 1
 | isAsciiUpper c = ord c - ord 'A' + 27
 | otherwise = 0

calculateValue line =
  let (pack1, pack2) = splitHalf line
      commonChars = intersect pack1 pack2
      char = head commonChars
      result = charToScore char
  in trace ("score: " ++ show result) result

solve1 :: String -> Int
solve1 input = do
  let scores = sum $ map calculateValue $ lines input
  scores

solve2 :: String -> Int
solve2 input = do
  23

day03 :: IO ()
day03 = do
  input <- getDataFileName "day03-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
