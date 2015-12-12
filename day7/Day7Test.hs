module Day7Test where

import Test.HUnit
import Day7

test1 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Val (Constant 123)) (Register "x")
            line = "123 -> x"

test2 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Op And (Register "x") (Register "y")) (Register "z")
            line = "x AND y -> z"

test3 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Op LShift (Register "p") (Constant 2)) (Register "q")
            line = "p LSHIFT 2 -> q"

test4 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Not (Register "e")) (Register "f")
            line = "NOT e -> f"

tests = TestList [ test1, test2, test3, test4 ]

main = runTestTT tests
