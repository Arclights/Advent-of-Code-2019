module Day9Tests where

import Data.List.Split
import Test.HUnit
import IntComputer

readData :: String -> IO [Int]
readData path = do
                  list <- readFile path
                  return $ map (\x -> read x :: Int) (splitOn "," list)

p1ex1 :: Test
p1ex1 = TestCase(do
                  let input = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
                  let result = snd $ calculate [] input
                  assertEqual "" (reverse [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]) result
                  )

p1ex2 :: Test
p1ex2 = TestCase(do
                  let input = [1102,34915192,34915192,7,4,7,99,0]
                  let result = snd $ calculate [] input
                  assertEqual "" [1219070632396864] result
                  )

p1ex3 :: Test
p1ex3 = TestCase(do
                  let input = [104,1125899906842624,99]
                  let result = snd $ calculate [] input
                  assertEqual "" [1125899906842624] result
                  )
                  
p1 :: Test
p1 = TestCase ( do
                  input <- readData "test/day9.txt"
                  let result = snd $ calculate [1] input
                  assertEqual "" [3989758265] result
                  )
                  
p2 :: Test
p2 = TestCase ( do
                  input <- readData "test/day9.txt"
                  let result = snd $ calculate [2] input
                  assertEqual "" [76791] result
                  )

day9Tests :: Test
day9Tests = test [ 
                   TestLabel "Part1 Example1" p1ex1,
                   TestLabel "Part1 Example2" p1ex2,
                   TestLabel "Part1 Example3" p1ex3,
                   TestLabel "Part1" p1,
                   TestLabel "Part2" p2
                 ]