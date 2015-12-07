module Day3Test where

import Test.HUnit
import Day3

test1 = TestCase (assertEqual path 2 $ santaRun(path)) where
            path = ">"
test2 = TestCase (assertEqual path 4 $ santaRun(path)) where
            path = "^>v<"
test3 = TestCase (assertEqual path 2 $ santaRun(path)) where
            path = "^v^v^v^v^v"

tests = TestList [ test1, test2, test3 ]

main = runTestTT tests
