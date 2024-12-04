module Day04 where

import Data.Bifunctor
import Debug.Trace (trace)
import Lib
import Paths_aoc (getDataFileName)

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

toCharList :: [String] -> [[Char]]
toCharList = id

buildGrid :: [String] -> Grid Char
buildGrid = toCharList

isXMAS :: [Char] -> Bool
isXMAS vals = vals == ['X', 'M', 'A', 'S']

checkCells :: Grid Char -> Int
checkCells grid =
  let maxRow = length grid - 1
      maxCol = length (head grid) - 1
      cells = [(row, col) | row <- [0 .. maxRow], col <- [0 .. maxCol]]
      adjacentLetters cell = filter (\vals -> length vals == 4) $ map (\(dr, dc) -> getValsAtSubsequentPoints grid cell (dr, dc) 3) allDirections
      allMatches = map adjacentLetters cells
      matchingCells = sum [length $ filter isXMAS (adjacentLetters cell) | cell <- cells]
   in trace ("allMatches:\n" ++ show allMatches) matchingCells

solve1 :: [String] -> Int
solve1 input =
  let grid = buildGrid input
      adjacentLetters = checkCells grid
   in trace (show adjacentLetters) adjacentLetters

{-
Part 2:
- Now we will check each A, if the cell is an A, we will check that the values in the diagonal directions match
  our expected output. They need to be in order. TOP LEFT , CELL , BOTTOM RIGHT and TOP RIGHT, CELL< BOTTOM LEFT.
- If we can't find these values we skip the cells. After doing this, if we get MAS and MAS we classify it as a WIN.
-}

-- Cross directions
crossDirections :: [(Int, Int)]
crossDirections = [(-1, -1), (0, 0), (1, 1), (-1, 1), (0, 0), (1, -1)]

hasXMAS :: Maybe ([Char], [Char]) -> Bool
hasXMAS cell = case cell of
  Just (diag1, diag2) -> all (`elem` diag1) ['M', 'S'] && all (`elem` diag2) ['M', 'S']
  _ -> False

checkCrossCells :: Grid Char -> Int
checkCrossCells grid =
  let maxRow = length grid - 1
      maxCol = length (head grid) - 1
      onlyAs = [(row, col) | row <- [0 .. maxRow], col <- [0 .. maxCol], getAt grid (row, col) == 'A']
      getDiagonalValues (r, c) =
        let points1 = filter (isValidPoint grid) [bimap (+ r) (+ c) (-1, -1), bimap (+ r) (+ c) (1, 1)]
            points2 = filter (isValidPoint grid) [bimap (+ r) (+ c) (-1, 1), bimap (+ r) (+ c) (1, -1)]
            vals1 = map (getAt grid) points1
            vals2 = map (getAt grid) points2
         in if length points1 == 2 && length points2 == 2 -- Only if we have all 4 points
              then Just (vals1, vals2)
              else Nothing
      allDiagonals = map getDiagonalValues onlyAs
      validDiagonals = filter hasXMAS allDiagonals
   in trace (show validDiagonals) length validDiagonals

solve2 :: [String] -> Int
solve2 input = do
  checkCrossCells input

day04 :: IO ()
day04 = do
  input <- getDataFileName "day04-input.txt" >>= readFile
  -- print $ solve1 (lines input)
  print $ solve2 (lines input)
