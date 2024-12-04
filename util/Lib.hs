module Lib where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

-- Graph operations

type Grid a = [[a]]

cardinalDirections :: [(Int, Int)]
cardinalDirections = [(-1, 0), (1, 0), (0, -1), (0, 1)]

allDirections :: [(Int, Int)]
allDirections = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]

gridDimensions :: Grid a -> (Int, Int)
gridDimensions grid = (maxRows, maxCols)
  where
    maxRows = length grid
    maxCols = length $ head grid

getAt :: Grid a -> (Int, Int) -> a
getAt grid (row, col) = (grid !! row) !! col

getNeighbors :: Grid a -> (Int, Int) -> [(Int, Int)] -> [a]
getNeighbors grid (row, col) = map (\(dr, dc) -> getAt grid (row + dr, col + dc))

getSubsequentPoints :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
getSubsequentPoints (row, col) (dr, dc) times =
  [(row + dr * n, col + dc * n) | n <- [0 .. times]]

getValsAtSubsequentPoints :: Grid a -> (Int, Int) -> (Int, Int) -> Int -> [a]
getValsAtSubsequentPoints grid (row, col) (dr, dc) times = do
  let points = getSubsequentPoints (row, col) (dr, dc) times
  let vals = map (\(r, c) -> getAt grid (r, c)) points
  vals

-- Input

strToIntList :: [String] -> [Int]
strToIntList = map read

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
