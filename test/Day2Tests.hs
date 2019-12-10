module Day2Tests where
import Test.HUnit
import Data.List.Split
import Day2

test1 :: Test
test1 = TestCase(do
                  let input = [1,9,10,3,2,3,11,0,99,30,40,50]
                  let result = part1 input
                  assertEqual "" 3500 result
                  ) 

test2 :: Test
test2 = TestCase(do
                  let input = [1,0,0,0,99]
                  let result = part1 input
                  assertEqual "" 2 result
                  ) 

test3 :: Test
test3 = TestCase(do
                  let input = [2,3,0,3,99]
                  let result = part1 input
                  assertEqual "" 2 result
                  ) 
                  
test4 :: Test
test4 = TestCase(do
                  let input = [2,4,4,5,99,0]
                  let result = part1 input
                  assertEqual "" 2 result
                  ) 

test5 :: Test
test5 = TestCase(do
                  let input = [1,1,1,4,99,5,6,0,99]
                  let result = part1 input
                  assertEqual "" 30 result
                  )

prep :: [Int] -> [Int]
prep state = replace (replace state 2 2) 1 12

test6 :: Test
test6 = TestCase ( do
                    list <- readFile "test/day2_part1.txt"
                    let input = map (\x -> read x :: Int) (splitOn "," list)
                    let result = part1 $ prep input
                    assertEqual "" 4930687 result
                    )
                    


day2Tests :: Test
day2Tests = test [ TestLabel "Test1" test1, 
                   TestLabel "Test2" test2, 
                   TestLabel "Test3" test3, 
                   TestLabel "Test4" test4, 
                   TestLabel "Test5" test5,
                   TestLabel "Test6" test6 ]