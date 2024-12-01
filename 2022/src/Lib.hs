module Lib where

import Data.Char (isDigit)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)
import Text.ParserCombinators.ReadP

-- Debug Print

-- Create a debug printing function that checks for DEBUG environment variable
debugPrint :: (Show a) => String -> a -> a
debugPrint label x = unsafePerformIO $ do
  debugMode <- lookupEnv "DEBUG"
  case debugMode of
    Just _ -> return $ trace (label ++ ": " ++ show x) x
    Nothing -> return x

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