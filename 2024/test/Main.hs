module Main where
import Test.Hspec
import Day01
import Day02
import Day03
import Day04

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    it "solves part 1" $ do
      input <- readFile "test/data/day01.txt"
      Day01.solve1 input `shouldBe` 11 
    it "solves part 2" $ do
      input <- readFile "test/data/day01.txt"
      Day01.solve2 input `shouldBe` 31 