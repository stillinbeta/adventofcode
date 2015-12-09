module Day6Test where

import Test.HUnit
import Day6

test1 = TestCase (assertEqual line expected $ parseLine line) where
            line = "turn on 0,0 through 999,999"
            expected = Instr TurnOn (Pt 0 0) (Pt 999 999)

test2 = TestCase (assertEqual line expected $ parseLine line) where
            line = "toggle 0,0 through 999,0"
            expected = Instr Toggle (Pt 0 0) (Pt 999 0)

test3 = TestCase (assertEqual line expected $ parseLine line) where
            line = "turn off 499,499 through 500,500"
            expected = Instr TurnOff (Pt 499 499) (Pt 500 500)

test4 = TestCase (assertEqual (show instr) 100000 $ lightsOn $ runLine instr mempty) where
            instr = Instr TurnOn (Pt 0 0) (Pt 999 999)

test5 = TestCase (assertEqual (show instr) 1000 $ lightsOn $ runLine instr mempty) where
            instr = Instr Toggle (Pt 0 0) (Pt 999 0)

tests = TestList [ test1, test2, test3, test4, test5 ]

main = runTestTT tests
