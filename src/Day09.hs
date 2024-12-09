module Day09 where

import Data.Map
import Debug.Trace (trace)
import Paths_aoc (getDataFileName)

{-
(file id, size, empty space)
12345

File ID: 0,1,2
0..111....22222

. is free space, we will want to fill it up, one block a a time
starting from the rightmost blocks until we have no more gaps remaining
between the file blocks

0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......

Finally, we calculate the checksum by multiplying the position by the file ID (e.g., 0)

Approach:
- Create a map with (pos:file_id)
  - Initialize it with the input line. It will have as many elements as the sum of (file id * size)
  we don't care about the empty space since we will fill it up
  - Empty spaces can have a "." value (check if possible in haskell)
- Reverse the input array after and try to fill up each of the empty spaces with
each number in the array, keep iterating until we find an empty space. Once we
do, move the array forward.
- We will end when we're done iterating over the input array (and map has been filled out?)
-}

-- [(id,size,empty space)]
toTuples :: [Char] -> [(Int, Char, Char)]
toTuples input = go input [] 0
  where
    go [] acc _ = reverse acc
    go (x : y : xs) acc id = go xs ((id, x, y) : acc) (id + 1)
    go _ acc id = reverse acc

-- Insert (id,size,empty) into map
-- We want to create a list out of these that has
-- (pos,id*size) and (pos...,'.' * empty)
-- e.g., if we have "(0,1,2)" we get [(0,'0'),(1,'.'),(2,'.') because
-- of pos = 0 and file id = 0
-- Note: We will need to pass to this function the latest position

-- build a map with: Int : Char with key: position, value: Either File ID or '.'
buildMap :: [(Int, Char, Char)] -> Map Int Char
buildMap tuples = go tuples Map.empty
  where
    go [] map = map
    go [(id, size, empty) : rest] map = map
    go _ map = map

-- Create a list of [(0..111... etc...)]
-- Then convert it to a map with (pos,file_id)
expandDisk :: [Char] -> [(Int, Char, Char)]
expandDisk input =
  let pairs = toTuples input
   in trace (show pairs) pairs

solve1 :: [Char] -> Int
solve1 input =
  let expandedDisk = expandDisk input
   in -- let diskMap = buildMap input
      trace (show expandedDisk) 23

solve2 :: [Char] -> Int
solve2 input =
  23

day09 :: IO ()
day09 = do
  input <- getDataFileName "day09-input.txt" >>= readFile
  print $ solve1 input

-- print $ solve2 input
