module Day1Tests where

import Test.HUnit
import Day1

makeTest :: String -> Int -> Test
makeTest parens expected = TestCase (assertEqual parens expected $ countParens(parens))

test1 = makeTest "(())"     0
test2 = makeTest "()()"     0
test3 = makeTest "((("      3
test4 = makeTest "(()(()("  3
test5 = makeTest "))((((("  3
test6 = makeTest ")))"    (-3)
test7 = makeTest ")))\n"  (-3)


tests = TestList [ test1, test2, test3, test4, test5, test6, test7 ]

main = runTestTT tests

