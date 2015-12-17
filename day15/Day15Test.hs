module Day15Test where

import Test.HUnit
import Day15

butterscotch = Ingredient { iName = "Butterscotch"
                          , iCapacity   = -1
                          , iDurability = -2
                          , iFlavour    = 6
                          , iTexture    = 3
                          , iCalories   = 8
                          }

cinnamon = Ingredient { iName = "Cinnamon"
                      , iCapacity   = 2
                      , iDurability = 3
                      , iFlavour    = -2
                      , iTexture    = -1
                      , iCalories   = 3
                      }

test1 = TestCase ( assertEqual string (Just butterscotch) $ parse string ) where
        string = "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"

test2 = TestCase ( assertEqual string (Just cinnamon) $ parse string ) where
        string = "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"

test3 = TestCase ( assertEqual "howDelicious" 62842880 $ deliciousness ingredients ) where
        ingredients = (replicate 44 butterscotch) ++ (replicate 56 cinnamon)

test4 = TestCase ( assertEqual "mostDelicious" 62842880 (maxRecipe $ deliciousIngredients [butterscotch, cinnamon] 100 ) ) where

tests = TestList [ test1, test2, test3, test4 ]

main = runTestTT tests
