module Day7Tests where
import Test.HUnit
import Day7

test1 :: Test
test1 = TestCase (do input <- readFile "test/day7_example1.txt"
                     let result = part1 input
                     assertEqual "" result 42)

test2 :: Test
test2 = TestCase (do input <- readFile "test/day7_part1.txt"
                     let result = part1 input
                     assertEqual "" result 417916)

day7Tests :: Test
day7Tests = test [ test1,
                    test2 ]