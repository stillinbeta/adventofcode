{-# LANGUAGE QuasiQuotes #-}

module Day12Test where

import Text.RawString.QQ
import Test.HUnit
import Day12

test1 = TestCase (assertEqual json 6 $ sumJSON json Nothing) where
        json = [r|[1,2,3]|]

test2 = TestCase (assertEqual json 6 $ sumJSON json Nothing) where
        json = [r|{"a":2,"b":4}|]

test3 = TestCase (assertEqual json 3 $ sumJSON json Nothing) where
        json = [r|[[[3]]]|]

test4 = TestCase (assertEqual json 3 $ sumJSON json Nothing) where
        json = [r|{"a":{"b":4},"c":-1}|]

test5 = TestCase (assertEqual json 0 $ sumJSON json Nothing) where
        json = [r|{"a":[-1,1]}|]

test6 = TestCase (assertEqual json 0 $ sumJSON json Nothing) where
        json = [r|[-1,{"a":1}]|]

test7 = TestCase (assertEqual json 0 $ sumJSON json Nothing) where
        json = [r|[]|]

test8 = TestCase (assertEqual json 0 $ sumJSON json Nothing) where
        json = [r|{}|]

test9 = TestCase (assertEqual json 4 $ sumJSON json (Just "red")) where
        json = [r|[1,{"c":"red","b":2},3]|]

test10 = TestCase (assertEqual json 0 $ sumJSON json (Just "red")) where
        json = [r|{"d":"red","e":[1,2,3,4],"f":5}|]


tests = TestList [ test1, test2, test3, test4,
                   test5, test6, test7, test8,
                   test9, test10  ]

main = runTestTT tests
