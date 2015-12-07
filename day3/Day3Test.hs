module Day3Test where

import Test.HUnit
import Day3

test1 = TestCase (assertEqual path 2 $ santaRun 1 path) where
            path = ">"
test2 = TestCase (assertEqual path 4 $ santaRun 1 path) where
            path = "^>v<"
test3 = TestCase (assertEqual path 2 $ santaRun 1 path) where
            path = "^v^v^v^v^v"
test4 = TestCase (assertEqual path 3 $ santaRun 2 path) where
            path = "^v"
test5 = TestCase (assertEqual path 3 $ santaRun 2 path) where
            path = "^>v<"
test6 = TestCase (assertEqual path 11 $ santaRun 2 path) where
            path = "^v^v^v^v^v"



tests = TestList [ test1, test2, test3, test4, test5, test6 ]

main = runTestTT tests
