module Day17Tests where

import Data.List.Split
import Test.HUnit
import Day17

readData :: String -> IO [Int]
readData path = do
                  list <- readFile path
                  return $ map (\x -> read x :: Int) (splitOn "," list)

p1ex1 :: Test
p1ex1 = TestCase(do
                  let input = [['.','.','#','.','.','.','.','.','.','.','.','.','.'],
                               ['.','.','#','.','.','.','.','.','.','.','.','.','.'],
                               ['#','#','#','#','#','#','#','.','.','.','#','#','#'],
                               ['#','.','#','.','.','.','#','.','.','.','#','.','#'],
                               ['#','#','#','#','#','#','#','#','#','#','#','#','#'],
                               ['.','.','#','.','.','.','#','.','.','.','#','.','.'],
                               ['.','.','#','#','#','#','#','.','.','.','^','.','.']]
                  let result = getIntersections input
                  assertEqual "" (reverse [(2, 2), (2, 4), (6, 4), (10, 4)]) result
                  )

p1ex2 :: Test
p1ex2 = TestCase(do
                  let input = [['.','.','#','.','.','.','.','.','.','.','.','.','.'],
                               ['.','.','#','.','.','.','.','.','.','.','.','.','.'],
                               ['#','#','#','#','#','#','#','.','.','.','#','#','#'],
                               ['#','.','#','.','.','.','#','.','.','.','#','.','#'],
                               ['#','#','#','#','#','#','#','#','#','#','#','#','#'],
                               ['.','.','#','.','.','.','#','.','.','.','#','.','.'],
                               ['.','.','#','#','#','#','#','.','.','.','^','.','.']]
                  let result = getIntersectionTotal input
                  assertEqual "" 76 result
                  )
                  
p1ex3 :: Test
p1ex3 = TestCase(do
                  let input = [35, 46, 10, 46, 35]
                  let result = parseImage input
                  assertEqual "" [['#','.'],['.','#']] result
                  )

p1 :: Test
p1 = TestCase ( do
                  input <- readData "test/day17.txt"
                  let result = part1 input
                  assertEqual "" 4600 result
                  )

day17Tests :: Test
day17Tests = test [ 
                   TestLabel "Part1 Example1" p1ex1,
                   TestLabel "Part1 Example2" p1ex2,
                   TestLabel "Part1 Example3" p1ex3,
                   TestLabel "Part1" p1
--                   TestLabel "Part2" p2
                 ]