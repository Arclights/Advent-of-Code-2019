module Main (main) where

import Test.HUnit
import Day2Tests
import Day7Tests

tests :: Test
tests = TestList [ day2Tests 
                    day7Tests ]

main :: IO Counts
main = runTestTT tests
