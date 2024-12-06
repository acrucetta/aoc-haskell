module Lib where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.Foldable (find)
import Data.Maybe

-- Graph parsing

toCharList :: [String] -> [[Char]]
toCharList = id

buildGrid :: [String] -> Grid Char
buildGrid = toCharList

-- Graph operations

type Grid a = [[a]]
data Point = Point Int Int

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

getAt :: Grid a -> (Int, Int) -> Maybe a
getAt grid (row, col) = 
  if isValidPoint grid (row,col)
  then Just ((grid !! row) !! col)
  else Nothing

getNeighbors :: Grid a -> (Int, Int) -> [(Int, Int)] -> [a]
getNeighbors grid (row, col) = mapMaybe (\(dr, dc) -> getAt grid (row + dr, col + dc))

getSubsequentPoints :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
getSubsequentPoints (row, col) (dr, dc) times =
  [(row + dr * n, col + dc * n) | n <- [0 .. times]]

isValidPoint :: Grid a -> (Int, Int) -> Bool
isValidPoint grid (row, col) =
  let (maxRows, maxCols) = gridDimensions grid
   in row >= 0 && row < maxRows && col >= 0 && col < maxCols

getValsAtSubsequentPoints :: Grid a -> (Int, Int) -> (Int, Int) -> Int -> [a]
getValsAtSubsequentPoints grid (row, col) (dr, dc) times =
  let potentialPoints = getSubsequentPoints (row, col) (dr, dc) times
      validPoints = filter (isValidPoint grid) potentialPoints
      vals = map (\(r, c) -> getAt grid (r, c)) validPoints
   in catMaybes vals

-- Graph find

-- Finds first ocurrence of element in 2D list
findCoords :: Eq a => a -> [[a]] -> Maybe (Int, Int)
findCoords x matrix = 
  let coords = [(row, col) | (row, xs) <- zip [0..] matrix, (col, val) <- zip [0..] xs, val == x]
  in case coords of
       [] -> Nothing
       (coord:_) -> Just coord


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
