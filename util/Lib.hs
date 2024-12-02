module Lib where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

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
