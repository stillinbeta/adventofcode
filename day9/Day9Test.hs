module Day9Test where

import Test.HUnit
import Day9

distances :: [Distance]
distances = [ Distance (Pair "London" "Dublin")  464
            , Distance (Pair "London" "Belfast") 518
            , Distance (Pair "Dublin" "Belfast") 141
            ]

test1 = TestCase (assertEqual "shortest" 605 $ minimum (routeLengths distances))

test2 = TestCase (assertEqual "longest" 982 $ maximum (routeLengths distances))

test3 = TestCase (assertEqual string (Just (Distance (Pair "London" "Dublin") 464)) $ parse string)
        where string = "London to Dublin = 464"

tests = TestList [ test1, test2, test3 ]

main = runTestTT tests
