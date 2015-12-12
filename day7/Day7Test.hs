module Day7Test where

import qualified Data.Map.Strict as Map
import Data.Either
import Test.HUnit
import Day7

test1 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Val (Constant 123)) (Wire "x")
            line = "123 -> x"

test2 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Op And (Source (Wire "x")) (Source (Wire "y"))) (Wire "z")
            line = "x AND y -> z"

test3 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Op LShift (Source (Wire "p")) (Constant 2)) (Wire "q")
            line = "p LSHIFT 2 -> q"

test4 = TestCase (assertEqual line (Right expected) $ parse line) where
            expected = Assign (Not (Source (Wire "e"))) (Wire "f")
            line = "NOT e -> f"

test5 = TestCase (assertEqual "example" (Right expected) $ solve instructions) where
            expected = Map.fromList [ ((Wire "d"),(Val (Constant 72)))
                                    , ((Wire "e"),(Val (Constant 507)))
                                    , ((Wire "f"),(Val (Constant 492)))
                                    , ((Wire "g"),(Val (Constant 114)))
                                    , ((Wire "h"),(Val (Constant 65412)))
                                    , ((Wire "i"),(Val (Constant 65079)))
                                    , ((Wire "x"),(Val (Constant 123)))
                                    , ((Wire "y"),(Val (Constant 456)))
                                    ]
            instructions = rights $ map parse [ "123 -> x"
                                             , "456 -> y"
                                             , "x AND y -> d"
                                             , "x OR y -> e"
                                             , "x LSHIFT 2 -> f"
                                             , "y RSHIFT 2 -> g"
                                             , "NOT x -> h"
                                             , "NOT y -> i"
                                             ]

tests = TestList [ test1, test2, test3, test4, test5 ]

main = runTestTT tests
