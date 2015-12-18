module Day16Test where

import Test.HUnit
import Day16

test1 = TestCase (assertEqual string (Right sue) $ parseSue string) where
   string = "Sue 5: akitas: 9, vizslas: 7, cars: 5"
   sue = (newSue 5) { _akitas = (Just 9), _vizslas = (Just 7), _cars = (Just 5)}

test2 = TestCase (assertEqual string (Right sue) $ parseSue string) where
   string = "Sue 152: pomeranians: 4, cars: 7, children: 1"
   sue = (newSue 152) { _pomeranians = (Just 4), _cars = (Just 7), _children = (Just 1)}

test3 = TestCase (assertEqual string (Right sue) $ parseSue string) where
   string = "Sue 249: cats: 4, vizslas: 5, pomeranians: 6"
   sue = (newSue 249) { _cats = (Just 4), _vizslas = (Just 5), _pomeranians = (Just 6)}

test4 = TestCase (assertEqual "compare 1" DefinitelyNot f) where
    f = compareSues compareAttr sue1 sue2
    sue1 = (newSue 249) { _cats = (Just 4), _vizslas = (Just 5), _pomeranians = (Just 6)}
    sue2 = (newSue 249) { _cats = (Just 17), _pomeranians = (Just 5), _cars = (Just 6)}

test5 = TestCase (assertEqual "compare 1" Perhaps f) where
    f = compareSues compareAttr sue1 sue2
    sue1 = (newSue 249) { _cats = (Just 4), _vizslas = (Just 5), _pomeranians = (Just 6)}
    sue2 = (newSue 249) { _cats = (Just 4), _pomeranians = (Just 6), _cars = (Just 6)}

tests = TestList [ test1, test2, test3, test4, test5 ]

main = runTestTT tests
