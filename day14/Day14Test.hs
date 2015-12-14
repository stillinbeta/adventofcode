module Day14Test where

import Test.HUnit
import Day14

comet  = Reindeer { velocity = 14, sprint = 10, rest = 127 }
dancer = Reindeer { velocity = 16, sprint = 11, rest = 162 }

test1 = TestCase ( assertEqual string (Just comet) $ parse string) where
        string = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."

test2 = TestCase ( assertEqual string (Just dancer) $ parse string) where
        string = "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

test3 = TestCase ( assertEqual "comet distance" 1120 $ runRunReindeer 1000 comet)

test4 = TestCase ( assertEqual "dancer distance" 1056 $ runRunReindeer 1000 dancer)

tests = TestList [ test1, test2, test3, test4 ]

main = runTestTT tests
