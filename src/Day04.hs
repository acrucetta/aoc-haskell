module Day04 where

import Lib
import Paths_aoc (getDataFileName)

toCharList :: [String] -> [[Char]]
toCharList = id

buildGrid :: [String] -> Grid Char
buildGrid = toCharList

{-
Steps:
- We have the grid
- Now we need to iterate over each cel
  - At each cel, check if its an X
  - If so, check for matches in all directions
  - If any of those directions have a succesful match
  i.e., X,M,A,S then return a 1, else return 0

For each cel, get all the points in each direction
Then check if any of those points match X,M,A,S including
  the current cel 
-}

isChristmas :: [Char] -> Bool
isChristmas vals = vals == "XMAS"

checkCells :: Grid Char -> Int
checkCells grid = do
  let maxRow = (-) 1 length grid 
  let maxCol = (-) 1 length $ head grid
  let cells = [(row,col)|row<-[0..maxRow],col<-[0..maxCol]]
  2
  

solve1 :: [String] -> Int
solve1 input = do
  let grid = buildGrid input
  23

solve2 :: [String] -> Int
solve2 input = do
  23

day04 :: IO ()
day04 = do
  input <- getDataFileName "day04-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
