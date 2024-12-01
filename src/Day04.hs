module Day04 where

import Control.Applicative
import Control.Monad
import Debug.Trace
import Lib
import Paths_aoc (getDataFileName)
import Text.ParserCombinators.ReadP

{-
In how many assignment pairs does one range fully contain the other?
2-4,6-8
2-3,4-5

Steps:
1. Parse input into (x1,y1,x2,y2)
2. Check if one is inside of the other, if so add 1 else 0
-}

-- Check if either range contains the other
-- e.g., is x1<=x2 and y1>=y2 or x2<=x1 and y2>=y1
isContained :: (Ord a, Ord b) => (a, b) -> (a, b) -> Bool
isContained (x1, y1) (x2, y2) =
  (x1 <= x2 && y1 >= y2) -- 1 contains 2
    || (x2 <= x1 && y2 >= y1) -- 2 contains 1

parseRange :: ReadP (Int, Int, Int, Int)
parseRange = do
  x1 <- parseNumber
  void $ char '-'
  y1 <- parseNumber
  void $ char ','
  x2 <- parseNumber
  void $ char '-'
  y2 <- parseNumber
  eof
  return (x1, y1, x2, y2)

checkRange :: String -> Int
checkRange line = do
  case readP_to_S parseRange line of
    [(result, "")] ->
      let (x1, y1, x2, y2) = result
          contained = isContained (x1, y1) (x2, y2)
       in trace (show ((x1, y1), (x2, y2), contained)) (if contained then 1 else 0)
    _ -> trace ("Failed to parse: " ++ line) 0

solve1 :: [Char] -> Int
solve1 input = do
  sum $ map checkRange $ lines input

solve2 :: [Char] -> Int
solve2 input = do
  23

day04 :: IO ()
day04 = do
  input <- getDataFileName "day04-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
