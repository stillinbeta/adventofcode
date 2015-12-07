module Day2Test where

import Test.HUnit
import Day2

test1 = TestCase (assertEqual area 58 $ presentArea(area))
        where area = "2x3x4"
test2 = TestCase (assertEqual area 43 $ presentArea(area))
        where area = "1x1x10"
test3 = TestCase (assertEqual "multiline" (58 + 43) $ allPresentArea("2x3x4\n1x1x10"))

tests = TestList [ test1, test2, test3 ]

main = runTestTT tests
