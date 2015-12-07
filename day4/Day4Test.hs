module Day4Test where

import Test.HUnit
import Day4

test1 = TestCase (assertBool key $ checkGuess 609043 key 5) where
            key = "abcdef"
test2 = TestCase (assertEqual key 609043 $ findHash key 5) where
            key = "abcdef"
test3 = TestCase (assertEqual key 1048970 $ findHash key 5) where
            key = "pqrstuv"


tests = TestList [ test1, test2, test3 ]

main = runTestTT tests
