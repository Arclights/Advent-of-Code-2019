module Day1Tests where

import Test.HUnit
import Day1

readData :: String -> IO [Int]
readData path = do
                  list <- readFile path
                  return $ map (\x -> read x :: Int) $ lines list

p1ex1 :: Test
p1ex1 = TestCase(do
                  let input = [12,14,1969,100756]
                  let result = part1 input
                  assertEqual "" 34241 result
                  )
                  
p1 :: Test
p1 = TestCase ( do
                  input <- readData "test/day1_part1.txt"
                  let result = part1 input
                  assertEqual "" 3560353 result
                  )

p2ex1 :: Test
p2ex1 = TestCase(do
                  let input = [14]
                  let result = part2 input
                  assertEqual "" 2 result
                  )
                  
p2ex2 :: Test
p2ex2 = TestCase(do
                  let input = [1969]
                  let result = part2 input
                  assertEqual "" 966 result
                  )
                  
p2ex3 :: Test
p2ex3 = TestCase(do
                  let input = [100756]
                  let result = part2 input
                  assertEqual "" 50346 result
                  )
                  
p2 :: Test
p2 = TestCase ( do
                  input <- readData "test/day1_part1.txt"
                  let result = part2 input
                  assertEqual "" 0 result
                  )
                  
day1Tests :: Test
day1Tests = test [ TestLabel "Part1 Example1" p1ex1, 
                   TestLabel "Part1 Input" p1,
                   TestLabel "Part2 Example1" p2ex1, 
                   TestLabel "Part2 Example2" p2ex2, 
                   TestLabel "Part2 Example3" p2ex3, 
                   TestLabel "Part2 Input" p2 ]