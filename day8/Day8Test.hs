{-# LANGUAGE QuasiQuotes #-}

module Day8Test where

import Text.RawString.QQ
import Test.HUnit
import Day8

test1 = TestCase (assertEqual string (2, 0) $ dataLength string)
        where string = [r|""|]
test2 = TestCase (assertEqual string (5, 3) $ dataLength string)
        where string = [r|"abc"|]
test3 = TestCase (assertEqual string (10, 7) $ dataLength string)
        where string = [r|"aaa\"aaa"|]
test4 = TestCase (assertEqual string (6, 1) $ dataLength string)
        where string = [r|"\x27"|]
test5 = TestCase (assertEqual string (6, 1) $ dataLength string)
        where string = [r|"\x27"|]
test6 = TestCase (assertEqual string expected $ stringLex string) where
        expected = [[r|"|], [r|\x2a|], "\\\\", "x", "2", "z", "b", "c", "\\\\", [r|"|]]
        string = [r|"\x2a\x2zbc\\"|]

tests = TestList [ test1, test2, test3, test4, test6 ]

main = runTestTT tests
