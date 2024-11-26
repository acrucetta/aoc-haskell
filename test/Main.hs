module Main where
import Test.Hspec
import Day01
import Day02

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    it "solves part 1" $ do
      input <- readFile "test/data/day01.txt"
      Day01.solve1 input `shouldBe` 24000
    it "solves part 2" $ do
      input <- readFile "test/data/day01.txt"
      Day01.solve2 input `shouldBe` 45000
  describe "Day02" $ do
    it "solves part 1" $ do
      input <- readFile "test/data/day02.txt"
      Day02.solve1 input `shouldBe` 15 
    it "solves part 2" $ do
      input <- readFile "test/data/day02.txt"
      Day02.solve2 input `shouldBe` 45000