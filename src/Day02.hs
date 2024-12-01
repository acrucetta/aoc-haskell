{-# LANGUAGE DerivingStrategies #-}

module Day02 where

import Paths_aoc (getDataFileName)
import Debug.Trace (trace)
import Prelude hiding (round)
import Data.Vector.Generic.Mutable (move)

data Move = Rock | Paper | Scissors | Undefined
  deriving (Show, Eq, Enum)

instance Ord Move where
  compare Rock Rock = EQ
  compare Paper Paper = EQ
  compare Scissors Scissors = EQ
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare _ _ = LT

letterToMove :: Char -> Move
letterToMove char = case char of
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors
  _ -> Undefined

calcRound :: Move -> Move -> Int
calcRound ours theirs = case compare ours theirs of
  GT -> 6
  LT -> 0
  EQ -> 3

getRoundOutcome :: String -> Int
getRoundOutcome round =
  let theirMove = letterToMove $ head round
      ourMove = letterToMove $ last round
      moveScore = fromEnum ourMove + 1
      result = calcRound ourMove theirMove + moveScore
  in result

solve1 :: [Char] -> Int
solve1 input = do
  let rounds = lines input
  sum (map getRoundOutcome rounds)

solve2 :: [Char] -> Int
solve2 input = do
  23

day02 :: IO ()
day02 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  print $ solve1 input
  print $ solve2 input
