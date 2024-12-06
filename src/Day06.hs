module Day06 where

import Paths_aoc (getDataFileName)
import Data.List.Split.Internals (build)
import Debug.Trace (trace)
import Lib
import Data.Maybe
import Data.Bifunctor

{-
Part 1

Steps:
- Find location of start node and direction
  - He will be facing up

- Move the node until we hit a # then turn right (we can create a grid that we keep iterating over)
  - [...]
- Keep moving until we hit a boundary, then keep track of all the visited nodes
- Return the total number of distinct nodes

Approach:
- Recursion with:
  - currNode, direction, visitedNodes

- peekNode
  - if out of bounds, return Nothing or something else
  - if in-bounds, check if its a "." or "#"
      - If it's # rotate the direction
      - If it's a . keep the same direction
-}


-- Function to rotate the direction 90 degrees to the right
rotateRight :: (Int, Int) -> (Int, Int)
rotateRight (dx, dy) = (dy, -dx)

-- Traverse the grid in a given direction
traverseGrid :: (Int, Int) -> (Int, Int) -> Grid Char -> [(Int, Int)] -> [(Int, Int)]
traverseGrid (x, y) (dx, dy) grid visited =
  case getAt grid (x+dx, y+dy)  of
    Just '#' -> trace("# -> Current coords:" ++ show (x,y)) traverseGrid (bimap (+x) (+y) (rotateRight(dx,dy))) (rotateRight (dx, dy)) grid visited
    Just '.' -> trace(". -> Current coords:" ++ show (x,y)) traverseGrid (x+dx, y+dy) (dx, dy) grid ((x, y) : visited)
    Just '^' -> trace("^ -> Current coords:" ++ show (x,y)) traverseGrid (x+dx, y+dy) (dx, dy) grid ((x, y) : visited) 
    Nothing  -> visited
    _ -> error "Unknown character"

solve1 :: [String] -> Int
solve1 input =
  let grid = buildGrid input
      startCoords = fromMaybe (-1,-1) $ findCoords '^' grid 
      visited = traverseGrid startCoords (-1,0) grid []
  in trace(show startCoords ++ show visited) length visited

solve2 :: String -> Int
solve2 input = do
  23

day06 :: IO ()
day06 = do
  input <- getDataFileName "day06-input.txt" >>= readFile
  print $ solve1 (lines input)
  -- print $ solve2 input
