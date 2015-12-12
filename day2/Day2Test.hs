module Day2Test where

import Test.HUnit
import Day2

test1 = TestCase (assertEqual area 58 $ (presentArea . presentParser) area)
        where area = "2x3x4"
test2 = TestCase (assertEqual area 43 $ (presentArea . presentParser) area)

test4 = TestCase (assertEqual (show present) 34 $ ribbonLength present) where
                present = Present 2 3 4

test5 = TestCase (assertEqual (show present) 14 $ ribbonLength present) where
                present = Present 1 1 10

tests = TestList [ test1, test2, test4, test5 ]

main = runTestTT tests
