module Lib where

import Data.Char (isDigit)
import Data.Maybe
import Text.ParserCombinators.ReadP

-- Subsets

subsets :: [a] -> Int -> [[a]]
subsets _ 0 = [[]]
subsets [] _ = []
subsets (x:a) k = map (x:) (subsets a (k-1)) ++ subsets a k

-- Graph parsing

toCharList :: [String] -> [[Char]]
toCharList = id

buildGrid :: [String] -> Grid Char
buildGrid = toCharList

-- Graph operations

type Grid a = [[a]]

-- North, east, south, west
cardinalDirections :: [(Int, Int)]
cardinalDirections = [(0, -1), (1, 0), (0, 1), (-1, 0)]

allDirections :: [(Int, Int)]
allDirections = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]

gridDimensions :: Grid a -> (Int, Int)
gridDimensions grid = (maxRows, maxCols)
  where
    maxRows = length grid
    maxCols = length $ head grid

-- Return list of values with their points e.g., [(a,(Int,Int))]

toGridValues :: Grid a -> [(a, (Int, Int))]
toGridValues grid = [(val, (row, col)) | (row, rowVals) <- zip [0 ..] grid, (col, val) <- zip [0 ..] rowVals]

-- Graph getters

getAt :: Grid a -> (Int, Int) -> Maybe a
getAt grid (row, col) =
  if isPointWithinBounds grid (row, col)
    then Just ((grid !! row) !! col)
    else Nothing

getNeighbors :: Grid a -> (Int, Int) -> [(Int, Int)] -> [a]
getNeighbors grid (row, col) = mapMaybe (\(dr, dc) -> getAt grid (row + dr, col + dc))

getSubsequentPoints :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
getSubsequentPoints (row, col) (dr, dc) times =
  [(row + dr * n, col + dc * n) | n <- [0 .. times]]

isPointWithinBounds :: Grid a -> (Int, Int) -> Bool
isPointWithinBounds grid (row, col) =
  let (maxRows, maxCols) = gridDimensions grid
   in row >= 0 && row < maxRows && col >= 0 && col < maxCols

getValsAtSubsequentPoints :: Grid a -> (Int, Int) -> (Int, Int) -> Int -> [a]
getValsAtSubsequentPoints grid (row, col) (dr, dc) times =
  let potentialPoints = getSubsequentPoints (row, col) (dr, dc) times
      validPoints = filter (isPointWithinBounds grid) potentialPoints
      vals = map (\(r, c) -> getAt grid (r, c)) validPoints
   in catMaybes vals

-- Graph replace

replaceElement :: Grid a -> (Int, Int) -> a -> Grid a
replaceElement grid (row, col) value =
  let oldRow = grid !! row
      modifiedRow = take col oldRow ++ [value] ++ drop (col + 1) oldRow
   in take row grid ++ [modifiedRow] ++ drop (row + 1) grid

-- Graph find

-- Finds first ocurrence of element in 2D list
findCoords :: (Eq a) => a -> [[a]] -> Maybe (Int, Int)
findCoords x matrix =
  let coords = [(row, col) | (row, xs) <- zip [0 ..] matrix, (col, val) <- zip [0 ..] xs, val == x]
   in case coords of
        [] -> Nothing
        (coord : _) -> Just coord

-- Points

type Point = (Int, Int)

subtractPoints :: Point -> Point -> Point
subtractPoints (x1, y1) (x2, y2) =
  (x2 - x1,y2 - y1)

-- Input

toIntList :: [String] -> [Int]
toIntList = map read

-- Parsers

digit :: ReadP Char
digit =
  satisfy isDigit

numbers :: Int -> ReadP Int
numbers d =
  fmap read (count d digit)

parseNumber :: ReadP Int
parseNumber = do
  digits <- many1 (satisfy isDigit)
  return $ read digits
