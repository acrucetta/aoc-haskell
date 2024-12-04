module Day03 where

import Debug.Trace (trace)
import Lib (strToIntList)
import Paths_aoc (getDataFileName)
import Text.Regex.TDFA
import Data.List

mulRegex :: String
mulRegex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

doRegex :: String
doRegex = "(do)[(][)]"

dontRegex :: String
dontRegex = "(don't)[(][)]"

numsRegex :: String
numsRegex = "[0-9]+"

combinedRegex :: String
combinedRegex = mulRegex ++ "|" ++ doRegex ++ "|" ++ dontRegex

matchMuls :: String -> [String]
matchMuls str = getAllTextMatches (str =~ mulRegex) :: [String]

matchNums :: String -> [String]
matchNums str = getAllTextMatches (str =~ numsRegex) :: [String]

matchOps :: String -> [String]
matchOps str = getAllTextMatches (str =~ combinedRegex) :: [String]

processOps :: [String] -> [(String, Bool)]
processOps ops = go ops True []
  where
    go [] _ acc = reverse acc
    go (op : rest) enabled acc
      | op == "do()" = go rest True acc
      | op == "don't()" = go rest False acc
      | otherwise = go rest enabled ((op, enabled) : acc)

calculateMul :: [String] -> Int
calculateMul nums = do
  let intList = strToIntList nums
  let mulResult = head intList * last intList
  mulResult

calculateRow :: String -> Int
calculateRow str = do
  let matches = matchMuls str
  let nums = map (calculateMul . matchNums) matches
  sum nums

isMul :: String -> Bool
isMul str = "mul(" `isPrefixOf` str

calculateRowWithState :: String -> Int
calculateRowWithState str = do
  let ops = matchOps str
  let processedOps = [op | (op, enabled) <- processOps ops, enabled]
  let nums = map (calculateMul . matchNums) processedOps
  sum nums

solve1 :: String -> Int
solve1 input = do
  let rows = lines input
  let totals = map calculateRow rows
  sum totals

-- here are two new instructions you'll need to handle:
-- The do() instruction enables future mul instructions.
-- The don't() instruction disables future mul instructions.
-- Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.

{-
Approach:
- Get list of all values [mul,do,dont,mul,mul] [x]
- Take muls until we hit a dont, then drop until we hit a do
-}

solve2 :: String -> Int
solve2 input = do
  let rows = lines input
  let totals = map calculateRowWithState rows
  sum totals

day03 :: IO ()
day03 = do
  input <- getDataFileName "day03-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
