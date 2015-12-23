import Data.List (unlines)

import Day23
import Test.HUnit

program = [ Inc A
          , Jio A 2
          , Tpl A
          , Inc A
          ]

test1 = TestCase (assertEqual "pasing example" (Right program) $ parseProgram string) where
    string = unlines [ "inc a"
                     , "jio a, +2"
                     , "tpl a"
                     , "inc a"
                     , ""
                     ]

test2 = TestCase (assertEqual "execution example" 2 $ runProgram program)

main = runTestTT $ TestList [test1, test2]
