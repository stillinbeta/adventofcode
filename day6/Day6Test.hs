module Day6Test where

import Test.HUnit
import Day6
import Debug.Trace
import Data.Foldable

test1 = TestCase (assertEqual line expected $ parseLine line) where
            line = "turn on 0,0 through 999,999"
            expected = Instr TurnOn (Pt 0 0) (Pt 999 999)

test2 = TestCase (assertEqual line expected $ parseLine line) where
            line = "toggle 0,0 through 999,0"
            expected = Instr Toggle (Pt 0 0) (Pt 999 0)

test3 = TestCase (assertEqual line expected $ parseLine line) where
            line = "turn off 499,499 through 500,500"
            expected = Instr TurnOff (Pt 499 499) (Pt 500 500)

test4 = TestCase (assertEqual (show instr) 1000000 $ lightsOn $ runInstruction instr newGrid) where
            instr = Instr TurnOn (Pt 0 0) (Pt 999 999)

test5 = TestCase (assertEqual (show instr) 1000 $ lightsOn $ runInstruction instr newGrid) where
            instr = Instr Toggle (Pt 0 0) (Pt 999 0)

test6 = TestCase (assertEqual "toggle + off " (1000000 - 4) $ lightsOn grid') where
            grid = runInstruction (Instr Toggle (Pt 0 0) (Pt 999 999)) newGrid
            grid' = runInstruction (Instr TurnOff (Pt 499 499) (Pt 500 500)) grid

test7 = TestCase (assertEqual "on + toggle" (86) $ lightsOn result') where
            instr = [Instr TurnOn (Pt 0 0) (Pt 9 9)
                    ,Instr Toggle (Pt 5 8) (Pt 8 10)
                    --,Instr TurnOff (Pt 0 0) (Pt 0 10)
                    ]
            result = foldr' runInstruction (newGrid' 10) instr
            result' = trace (show result) result


--tests = TestList [ test1, test2, test3, test4, test5, test6, test7 ]
tests = TestList [  test7 ]

main = runTestTT tests
