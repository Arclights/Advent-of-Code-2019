module Day5Tests where

import Data.List.Split
import Test.HUnit
import Day5

readData :: String -> IO [Int]
readData path = do
                  list <- readFile path
                  return $ map (\x -> read x :: Int) (splitOn "," list)

p1ex1 :: Test
p1ex1 = TestCase(do
                  let input = [3,0,4,0,99]
                  let result = part1 [1] input
                  assertEqual "" (1, [1]) result
                  )
                  
p1ex2 :: Test
p1ex2 = TestCase(do
                  let input = [1002,4,3,4,33]
                  let result = part1 [] input
                  assertEqual "" (1002, []) result
                  )

p1 :: Test
p1 = TestCase ( do
                  input <- readData "test/day1_part1.txt"
                  let result = part1 [1] input
                  assertEqual "" (0, []) result
                  )

day5Tests :: Test
day5Tests = test [ TestLabel "Part1 Example1" p1ex1, 
                   TestLabel "Part1 Input" p1 ]