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
                  input <- readData "test/day5.txt"
                  let result = part1 [1] input
                  assertEqual "" (3, [5577461,0,0,0,0,0,0,0,0,0]) result
                  )

p2ex1 :: Test
p2ex1 = TestCase(do
                  let input = [3,9,8,9,10,9,4,9,99,-1,8]
                  let result = part1 [8] input
                  assertEqual "" (3, [1]) result
                  )
                  
p2ex2 :: Test
p2ex2 = TestCase(do
                  let input = [3,9,8,9,10,9,4,9,99,-1,8]
                  let result = part1 [9] input
                  assertEqual "" (3, [0]) result
                  )
                  
p2ex3 :: Test
p2ex3 = TestCase(do
                  let input = [3,9,7,9,10,9,4,9,99,-1,8]
                  let result = part1 [7] input
                  assertEqual "" (3, [1]) result
                  )
                  
p2ex4 :: Test
p2ex4 = TestCase(do
                  let input = [3,9,7,9,10,9,4,9,99,-1,8]
                  let result = part1 [9] input
                  assertEqual "" (3, [0]) result
                  )
                  
p2ex5 :: Test
p2ex5 = TestCase(do
                  let input = [3,3,1108,-1,8,3,4,3,99]
                  let result = part1 [8] input
                  assertEqual "" (3, [1]) result
                  )
                  
p2ex6 :: Test
p2ex6 = TestCase(do
                  let input = [3,3,1108,-1,8,3,4,3,99]
                  let result = part1 [9] input
                  assertEqual "" (3, [0]) result
                  )
                  
p2ex7 :: Test
p2ex7 = TestCase(do
                  let input = [3,3,1107,-1,8,3,4,3,99]
                  let result = part1 [7] input
                  assertEqual "" (3, [1]) result
                  )
                  
p2ex8 :: Test
p2ex8 = TestCase(do
                  let input = [3,3,1107,-1,8,3,4,3,99]
                  let result = part1 [9] input
                  assertEqual "" (3, [0]) result
                  )

p2ex9 :: Test
p2ex9 = TestCase(do
                  let input = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
                  let result = part1 [0] input
                  assertEqual "" (3, [0]) result
                  )
                  
p2ex10 :: Test
p2ex10 = TestCase(do
                  let input = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
                  let result = part1 [9] input
                  assertEqual "" (3, [1]) result
                  )     
                               
p2ex11 :: Test
p2ex11 = TestCase(do
                  let input = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
                  let result = part1 [0] input
                  assertEqual "" (3, [0]) result
                  )
                  
p2ex12 :: Test
p2ex12 = TestCase(do
                  let input = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
                  let result = part1 [9] input
                  assertEqual "" (3, [1]) result
                  )

p2ex13 :: Test
p2ex13 = TestCase(do
                  let input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                  let result = part1 [7] input
                  assertEqual "" (3, [999]) result
                  )
                  
p2ex14 :: Test
p2ex14 = TestCase(do
                  let input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                  let result = part1 [1] input
                  assertEqual "" (3, [999]) result
                  )  
                  
p2ex15 :: Test
p2ex15 = TestCase(do
                  let input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                  let result = part1 [8] input
                  assertEqual "" (3, [1000]) result
                  )   
                  
p2ex16 :: Test
p2ex16 = TestCase(do
                  let input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                  let result = part1 [9] input
                  assertEqual "" (3, [1001]) result
                  )  
                  
p2ex17 :: Test
p2ex17 = TestCase(do
                  let input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                  let result = part1 [100] input
                  assertEqual "" (3, [1001]) result
                  )
                  
p2 :: Test
p2 = TestCase ( do
                  input <- readData "test/day5.txt"
                  let result = part1 [5] input
                  assertEqual "" (314, [7161591]) result
                  )         

day5Tests :: Test
day5Tests = test [ TestLabel "Part1 Example1" p1ex1,
                   TestLabel "Part1 Input" p1,
                   TestLabel "Part2 Example1" p2ex1,
                   TestLabel "Part2 Example2" p2ex2,
                   TestLabel "Part2 Example3" p2ex3,
                   TestLabel "Part2 Example4" p2ex4,
                   TestLabel "Part2 Example5" p2ex5,
                   TestLabel "Part2 Example6" p2ex6,
                   TestLabel "Part2 Example7" p2ex7,
                   TestLabel "Part2 Example8" p2ex8,
                   TestLabel "Part2 Example9" p2ex9,
                   TestLabel "Part2 Example10" p2ex10,
                   TestLabel "Part2 Example11" p2ex11,
                   TestLabel "Part2 Example12" p2ex12,
                   TestLabel "Part2 Example13" p2ex13,
                   TestLabel "Part2 Example14" p2ex14,
                   TestLabel "Part2 Example15" p2ex15,
                   TestLabel "Part2 Example16" p2ex16,
                   TestLabel "Part2 Example17" p2ex17,
                   TestLabel "Part2 Input" p2
                 ]