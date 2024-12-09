module Day08 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace (trace)
import Lib
import Lib hiding (Point)
import Paths_aoc (getDataFileName)
import Text.Parsec (char)

{-
Steps:
- Get the list of all coordinates and their symbol
  - Focus on coordinates with a value of: any lowercase, uppercase letter, or digit from 0-9
  - Assign each letter 'a' to a list of coordinates.
- For each list of coordinates, two coordinates will interact
two create antinodes. We need to create pairs of N choose 2 for
all possible combinations.
- For each pair of coordinates, we will compute a formula
and check (1) is the node within bounds, and (2) where would it
be placed in the board. If possible to place in the board, save
the location where its possible and then we will get only
the unique valid locations.

Formula for antinodes:

"In particular, an antinode occurs at any point that is perfectly in line with two antennas of the same frequency - but only when one of the antennas is twice as far away as the other. This means that for any pair of antennas with the same frequency, there are two antinodes, one on either side of them."

(dx,dy)
(Ax-dx, Ax-dy)
(Ax+dx, Ax+dy)
(Bx+dx, Bx+dy)
(Bx-dx, Bx-dy)

Filter out those equal to Ax or Bx or out of bounds.
-}

buildMap :: [(Char, Point)] -> Map Char [Point]
buildMap = foldr insertCoord Map.empty
  where
    insertCoord (ch, coord) = Map.insertWith (++) ch [coord]

calculateAntinode :: Point -> Point -> Point
calculateAntinode (ax, ay) (bx, by) =
  let newX = bx + (bx - ax)
      newY = by + (by - ay)
   in (newX, newY)

-- Create all possible coordinate combinations,
-- calculate antinodes, filter them out if they're in coords
getAntinodes :: [Point] -> (Int, Int) -> [Point]
getAntinodes coords (maxRow, maxCol) =
  let perms = subsets coords 2
      antinodes = [calculateAntinode p1 p2 | [p1, p2] <- perms] ++ [calculateAntinode p2 p1 | [p1, p2] <- perms]
      filteredNodes = filter (\(x, y) -> 0 <= x && x < maxRow && 0 <= y && y < maxCol) antinodes
   in trace (show filteredNodes) filteredNodes

solve1 :: [String] -> Int
solve1 input =
  -- Find coordinates of each letter, store into: [(char, Point)]
  let grid = buildGrid input
      newGrid = toGridValues grid
      charMap = buildMap newGrid
      (maxRow, maxCol) = gridDimensions grid
      allAntinodes = (nub . concat) [getAntinodes nodes (maxRow, maxCol) | (ch, nodes) <- Map.toList charMap, ch /= '.']
   in trace (show allAntinodes) length allAntinodes

-- Keep generating antinodes until we're out of bounds, they will all be of the same distance
calculateAntinodePt2 :: Point -> Point -> (Int, Int) -> [Point]
calculateAntinodePt2 (ax, ay) (bx, by) (maxRow, maxCol) =
  takeWhile (\(x, y) -> 0 <= x && x < maxRow && 0 <= y && y < maxCol) $
    iterate (\(x, y) -> (x + (bx - ax), y + (by - ay))) (bx, by)

-- Create all possible coordinate combinations,
-- calculate antinodes, filter them out if they're in coords
getAntinodesPt2 :: [Point] -> (Int, Int) -> [Point]
getAntinodesPt2 coords (maxRow, maxCol) =
  let perms = subsets coords 2
      antinodes = concat [calculateAntinodePt2 p1 p2 (maxRow, maxCol) | [p1, p2] <- perms] ++ concat [calculateAntinodePt2 p2 p1 (maxRow, maxCol) | [p1, p2] <- perms]
      filteredNodes = filter (\(x, y) -> 0 <= x && x < maxRow && 0 <= y && y < maxCol) antinodes
   in trace (show filteredNodes) filteredNodes

solve2 :: [String] -> Int
solve2 input =
  let grid = buildGrid input
      newGrid = toGridValues grid
      charMap = buildMap newGrid
      (maxRow, maxCol) = gridDimensions grid
      allAntinodes = (nub . concat) [getAntinodesPt2 nodes (maxRow, maxCol) | (ch, nodes) <- Map.toList charMap, ch /= '.']
   in trace (show allAntinodes) length allAntinodes

day08 :: IO ()
day08 = do
  input <- getDataFileName "day08-input.txt" >>= readFile
  -- print $ solve1 (lines input)
  print $ solve2 (lines input)
