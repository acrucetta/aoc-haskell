{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day07 where

import qualified Control.Monad
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Lib (toIntList)
import Paths_aoc (getDataFileName)
import Prelude hiding ((||))
import Data.Char (intToDigit)

type Op = Int -> Int -> Int

partOneOps :: [(String, Op)]
partOneOps = [("+", (+)), ("*", (*))]

(||) :: Int -> Int -> Int
(||) a b = read (show a ++ show b) :: Int

partTwoOps :: [(String, Op)]
partTwoOps = [("+", (+)), ("*", (*)),("||",(||))]

applySeq :: [(String, Op)] -> [Int] -> Int
applySeq [] (x : _) = x
applySeq ((_, op) : ops) (x : y : rest) = applySeq ops (op x y : rest)
applySeq _ _ = error "No more numbers"

isValid :: (Int, [Int]) -> Bool
isValid (target, nums) =
  -- We will have N-1 possible operations given N is the count of numbers
  let countOps = length nums - 1
      combinations = Control.Monad.replicateM countOps partTwoOps
      validCombinations = [applySeq combo nums == target | combo <- combinations]
   in any id validCombinations

solve1 :: [String] -> Int
solve1 input =
  let rows = map (splitOn ":") input
      equations = [(read $ head row :: Int, (toIntList . words) $ last row) | row <- rows]
      validTargets = map snd $ [(isValid eqn, fst eqn) | eqn <- equations, isValid eqn]
   in trace (show validTargets) sum validTargets 

solve2 :: [String] -> Int
solve2 input = do
  let rows = map (splitOn ":") input
      equations = [(read $ head row :: Int, (toIntList . words) $ last row) | row <- rows]
      validTargets = map snd $ [(isValid eqn, fst eqn) | eqn <- equations, isValid eqn]
   in trace (show validTargets) sum validTargets 

day07 :: IO ()
day07 = do
  input <- getDataFileName "day07-input.txt" >>= readFile
  -- print $ solve1 $ lines input
  print $ solve2 $ lines input
