import Test.HUnit
import Day7

test1 :: Test
test1=TestCase (assertEqual "" True True)

test2 :: Test
test2 = TestCase (do input <- readFile "test/day7_example1.txt"
                     let result = part1 input
                     assertEqual "" result 42)

test3 :: Test
test3 = TestCase (do input <- readFile "test/day7_part1.txt"
                     let result = part1 input
                     assertEqual "" result 417916)

tests :: Test
tests = TestList [
  TestLabel "Test1" test1,
   TestLabel "Day 7 part 1 example" test2,
   TestLabel "Day 7 part 1" test3]

main :: IO Counts
main = runTestTT tests
