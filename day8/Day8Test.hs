{-# LANGUAGE QuasiQuotes #-}

module Day8Test where

import Text.RawString.QQ
import Test.HUnit
import Day8

test1 = TestCase (assertEqual string (CodeDiff 2 0) $ dataLength string)
        where string = [r|""|]
test2 = TestCase (assertEqual string (CodeDiff 5 3) $ dataLength string)
        where string = [r|"abc"|]
test3 = TestCase (assertEqual string (CodeDiff 10 7) $ dataLength string)
        where string = [r|"aaa\"aaa"|]
test4 = TestCase (assertEqual string (CodeDiff 6 1) $ dataLength string)
        where string = [r|"\x27"|]
test5 = TestCase (assertEqual string (CodeDiff 6 1) $ dataLength string)
        where string = [r|"\x27"|]
test6 = TestCase (assertEqual string expected $ stringLex string) where
        expected = [[r|"|], [r|\x2a|], "\\\\", "x", "2", "z", "b", "c", "\\\\", [r|"|]]
        string = [r|"\x2a\x2zbc\\"|]

test7 = TestCase (assertEqual string expected $ escape string) where
        expected = [r|"\"\""|]
        string = [r|""|]
test8 = TestCase (assertEqual string expected $ escape string) where
        expected = [r|"\"abc\""|]
        string = [r|"abc"|]
test9 = TestCase (assertEqual string expected $ escape string) where
        expected = [r|"\"aaa\\\"aaa\""|]
        string = [r|"aaa\"aaa"|]
test10 = TestCase (assertEqual string expected $ escape string) where
        expected = [r|"\"\\x27\""|]
        string = [r|"\x27"|]


tests = TestList [ test1, test2, test3, test4, test5,
                   test6, test7, test8, test9, test10 ]

main = runTestTT tests
