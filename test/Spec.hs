module Main (main) where

import Test.HUnit
import Day1Tests
import Day2Tests
import Day5Tests
import Day7Tests
import Day9Tests
import Day17Tests

tests :: Test
tests = TestList [ 
                    day1Tests,
                    day2Tests,
                    day5Tests,
                    day7Tests,
                    day9Tests,
                    day17Tests
                 ]

main :: IO Counts
main = runTestTT tests
