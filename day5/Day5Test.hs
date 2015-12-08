module Day5Test where

import Test.HUnit
import Day5

test1 = TestCase (assertEqual word True $ isNice word) where
            word = "ugknbfddgicrmopn"
test2 = TestCase (assertEqual word True $ isNice word) where
            word = "aaa"
test3 = TestCase (assertEqual word False $ isNice word) where
            word = "jchzalrnumimnmhp"
test4 = TestCase (assertEqual word False $ isNice word) where
            word = "haegwjzuvuyypxyu"
test5 = TestCase (assertEqual word False $ isNice word) where
            word = "dvszwmarrgswjxmb"


tests = TestList [ test1, test2, test3, test4, test5 ]

main = runTestTT tests
