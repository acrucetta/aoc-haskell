module Day06 where

import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace (trace)
import Lib
import Paths_aoc (getDataFileName)
import Data.List

-- Function to rotate the direction 90 degrees to the right
rotateRight :: (Int, Int) -> (Int, Int)
rotateRight (dx, dy) = (dy, -dx)

-- Traverse the grid in a given direction
traverseGrid :: (Int, Int) -> (Int, Int) -> Grid Char -> [(Int, Int)] -> [(Int, Int)]
traverseGrid pos@(x, y) dir@(dx, dy) grid visited =
  case getAt grid nextPos of
    Just '#' -> trace ("# -> Current coords:" ++ show pos) traverseGrid pos (rotateRight dir) grid (pos : visited)
    Just '.' -> trace (". -> Current coords:" ++ show pos) traverseGrid nextPos dir grid (pos : visited)
    Just '^' -> trace ("^ -> Current coords:" ++ show pos) traverseGrid nextPos dir grid (pos : visited)
    Nothing -> visited
    _ -> error "Unknown character"
  where
    nextPos = (x + dx, y + dy)

solve1 :: [String] -> Int
solve1 input =
  let grid = buildGrid input
      startCoords = fromMaybe (-1, -1) $ findCoords '^' grid
      visited = traverseGrid startCoords (-1, 0) grid []
   in trace (show startCoords ++ show visited) (length $ nub visited) + 1

type Point = (Int,Int)
type Direction = (Int,Int)
type Path = Set.Set (Point, Direction)

-- Traverse the grid in a given direction
containsLoop :: Point -> Direction -> Grid Char -> Path -> Bool
containsLoop pos@(x,y) dir@(dx, dy) grid visited
  | Set.member (pos, dir) visited  = trace "...found loop!" True
  | otherwise =
      case getAt grid nextPos of
        Just '#' -> containsLoop pos (rotateRight dir) grid (Set.insert (pos,dir) visited)
        Just '.' -> containsLoop nextPos dir grid (Set.insert (pos,dir) visited)
        Just '^' -> containsLoop nextPos dir grid (Set.insert (pos,dir) visited)
        Nothing -> False
        _ -> error "Unknown character"
  where
    nextPos = (x + dx, y + dy)

checksForLoop :: Point -> Direction -> Grid Char -> Point -> Bool
checksForLoop start dir grid testPos = trace("Done checking for loop at:" ++ show testPos) containsLoop start dir (replaceElement grid testPos '#') Set.empty

getValidPoints :: Grid Char -> (Int,Int) -> [(Int,Int)]
getValidPoints grid start =
  let (maxRow, maxCol) = gridDimensions grid
      validNodes = [(r,c)| r<-[0..maxRow-1],c<-[0..maxCol-1], (r,c)/=start, getAt grid (r,c) == Just '.']
  in validNodes

solve2 :: [String] -> Int
solve2 input =
  let grid = buildGrid input
      start = fromMaybe (-1, -1) $ findCoords '^' grid
      candidates = getValidPoints grid start
      loopingPositions = filter (checksForLoop start (0,-1) grid) candidates
  in trace (show loopingPositions) length loopingPositions

day06 :: IO ()
day06 = do
  input <- getDataFileName "day06-input.txt" >>= readFile
  -- print $ solve1 (lines input)
  print $ solve2 (lines input)
