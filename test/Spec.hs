module Main (main) where

import Test.HUnit
import Day7Tests

tests :: Test
tests = TestList [ day7Tests ]

main :: IO Counts
main = runTestTT tests
