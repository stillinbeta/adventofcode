module Day11Test where

import Test.HUnit
import Day11

test1 = TestCase (assertEqual string  False $ validPass string) where
        string = "hijklmmn"

test2 = TestCase (assertEqual string  False $ validPass string) where
        string = "abbceffg"

test3 = TestCase (assertEqual string  False $ validPass string) where
        string = "abbcegjk"

test4 = TestCase (assertEqual string  True $ validPass string) where
        string = "abcdffaa"

test5 = TestCase (assertEqual string  True $ validPass string) where
        string = "ghjaabcc"

test6 = TestCase (assertEqual "nextpass" "aaaaaaya" $ nextPass "aaaaaaxz")

test7 = TestCase (assertEqual initial "abcdffaa" $ nextValidPass initial) where
        initial = "abcdefgh"

test8 = TestCase (assertEqual initial "ghjaabcc" $ nextValidPass initial) where
        initial = "ghijklmn"


tests = TestList [ test1, test2, test3, test4,
                   test5, test6, test7, test8 ]

main = runTestTT tests
