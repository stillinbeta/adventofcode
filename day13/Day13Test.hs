module Day13Test where

import Test.HUnit
import Day13

test1 = TestCase ( assertEqual string (Just expected) $ parse string) where
        expected = Happiness "Alice" "Bob" 54
        string = "Alice would gain 54 happiness units by sitting next to Bob."

test2 = TestCase ( assertEqual string (Just expected) $ parse string) where
        expected = Happiness "Carol" "Alice" (-62)
        string = "Carol would lose 62 happiness units by sitting next to Alice."

happiness = [ Happiness "Alice" "Bob"     54
            , Happiness "Alice" "Carol" (-79)
            , Happiness "Alice" "David" (-2)
            , Happiness "Bob"   "Alice"   83
            , Happiness "Bob"   "Carol" (-7)
            , Happiness "Bob"   "David" (-63)
            , Happiness "Carol" "Alice" (-62)
            , Happiness "Carol" "Bob"     60
            , Happiness "Carol" "David"   55
            , Happiness "David" "Alice"   46
            , Happiness "David" "Bob"   (-7)
            , Happiness "David" "Carol"   41
            ]

test3 = TestCase ( assertEqual "alice, bob, and carol" 330 $ findOptimal happiness)


tests = TestList [ test1, test2, test3 ]

main = runTestTT tests
