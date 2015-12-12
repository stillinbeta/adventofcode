module Day7Test where

import Test.HUnit
import Day7

test1 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Val (Constant 123)) (Wire "x")
            line = "123 -> x"

test2 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Op And (Source (Wire "x")) (Source (Wire "y"))) (Wire "z")
            line = "x AND y -> z"

test3 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Op LShift (Source (Wire "p")) (Constant 2)) (Wire "q")
            line = "p LSHIFT 2 -> q"

test4 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Not (Source (Wire "e"))) (Wire "f")
            line = "NOT e -> f"

tests = TestList [ test1, test2, test3, test4 ]

main = runTestTT tests
