module Main (main) where

import Test.HUnit
import Day1Tests
import Day2Tests
import Day5Tests
import Day7Tests

tests :: Test
tests = TestList [  --day1Tests,
                    day2Tests,
                    day5Tests ]
--                     day7Tests ]

main :: IO Counts
main = runTestTT tests
