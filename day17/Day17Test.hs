module Day17Test where

import Data.List (sort)
import Test.HUnit
import Day17

containers = [20, 15, 10, 5, 5]

test1 = let expected = [[5,5,15],[5,20],[5,20],[10,15]]
            results = waysToFill 25 containers
            resultsSorted = sort $ map sort results in
               TestCase (assertEqual "part a example" expected resultsSorted) where

test2 = TestCase (assertEqual "part b example" 3 $ length (waysToFillN 25 containers 2))

tests = TestList [ test1, test2 ]

main = runTestTT tests
