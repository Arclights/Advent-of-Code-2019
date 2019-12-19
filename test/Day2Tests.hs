module Day2Tests where
import Test.HUnit
import Data.List.Split
import Day2
import IntComputer

p1ex1 :: Test
p1ex1 = TestCase(do
                  let program = [1,9,10,3,2,3,11,0,99,30,40,50]
                  let (result, _) = calculate [] program
                  assertEqual "" 3500 result
                  ) 

p1ex2 :: Test
p1ex2 = TestCase(do
                  let program = [1,0,0,0,99]
                  let (result, _) = calculate [] program
                  assertEqual "" 2 result
                  ) 

p1ex3 :: Test
p1ex3 = TestCase(do
                  let program = [2,3,0,3,99]
                  let (result, _) = calculate [] program
                  assertEqual "" 2 result
                  ) 
                  
p1ex4 :: Test
p1ex4 = TestCase(do
                  let program = [2,4,4,5,99,0]
                  let (result, _) = calculate [] program
                  assertEqual "" 2 result
                  ) 

p1ex5 :: Test
p1ex5 = TestCase(do
                  let program = [1,1,1,4,99,5,6,0,99]
                  let (result, _) = calculate [] program
                  assertEqual "" 30 result
                  )

readData :: String -> IO [Int]
readData path = do
                  list <- readFile path
                  return $ map (\x -> read x :: Int) (splitOn "," list)

p1 :: Test
p1 = TestCase ( do
                  program <- readData "test/day2_part1.txt"
                  let result = part1 program
                  assertEqual "" 4930687 result
                  )
                    
p2ex1 :: Test
p2ex1 = TestCase(do
                  let input1 = 4930687
                  program <- readData "test/day2_part1.txt"
                  let result = part2 input1 program
                  assertEqual "" 1202 result
                  )
                  
p2 :: Test
p2 = TestCase(do
                let input1 = 19690720
                program <- readData "test/day2_part1.txt"
                let result = part2 input1 program
                assertEqual "" 5335 result
                )

day2Tests :: Test
day2Tests = test [ TestLabel "Part1 Example1" p1ex1,
                   TestLabel "Part1 Example2" p1ex2,
                   TestLabel "Part1 Example3" p1ex3,
                   TestLabel "Part1 Example4" p1ex4,
                   TestLabel "Part1 Example5" p1ex5,
                   TestLabel "Part1 Input" p1,
                   TestLabel "Part2 Example1" p2ex1,
                   TestLabel "Part2 Input" p2
                   ]