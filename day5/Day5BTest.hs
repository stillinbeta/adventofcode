module Day5BTest where

import Test.HUnit
import Day5B

test1 = TestCase (assertEqual word True $ isNice word) where
            word = "qjhvhtzxzqqjkmpb"
test2 = TestCase (assertEqual word True $ isNice word) where
            word = "xxyxx"
test3 = TestCase (assertEqual word False $ isNice word) where
            word = "uurcxstgmygtbstg"
test4 = TestCase (assertEqual word False $ isNice word) where
            word = "ieodomkazucvgmuy"
test5 = TestCase (assertEqual word False $ isNice word) where
            word = "aaa"

tests = TestList [ test1, test2, test3, test4, test5 ]

main = runTestTT tests
