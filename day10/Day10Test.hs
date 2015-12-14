module Day10Test where

import Test.HUnit
import Day10

test1 = TestCase (assertEqual string  "11" $ lookAndSay string) where
        string = "1"

test2 = TestCase (assertEqual string  "21" $ lookAndSay string) where
        string = "11"

test3 = TestCase (assertEqual string  "1211" $ lookAndSay string) where
        string = "21"

test4 = TestCase (assertEqual string  "111221" $ lookAndSay string) where
        string = "1211"

test5 = TestCase (assertEqual string  "312211" $ lookAndSay string) where
        string = "111221"

test6 = TestCase (assertEqual ("chained" ++ string) "312211" $ repeatedLookAndSay 5 string) where
        string = "1"

tests = TestList [ test1, test2, test3, test4, test5, test6 ]

main = runTestTT tests
