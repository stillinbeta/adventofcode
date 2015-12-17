module Day15 ( parse, Ingredient(..), deliciousness, maxRecipe, deliciousIngredients) where

import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import qualified Text.Read as Read

data Ingredient = Ingredient { iName       :: String
                             , iCapacity   :: Int
                             , iDurability :: Int
                             , iFlavour    :: Int
                             , iTexture    :: Int
                             , iCalories   :: Int
                             } deriving (Show, Eq)

parse :: String -> Maybe Ingredient
parse = parse' . words

parse' :: [String] -> Maybe Ingredient
parse' [name
       , "capacity", capacity
       , "durability", durability
       , "flavor", flavour
       , "texture", texture
       , "calories", calories
       ] = do
              -- Butterscotch:
              let name' = init name
              -- 17,
              capacity'   <- Read.readMaybe $ init capacity
              durability' <- Read.readMaybe $ init durability
              flavour'    <- Read.readMaybe $ init flavour
              texture'    <- Read.readMaybe $ init texture
              calories'   <- Read.readMaybe $ calories
              return Ingredient { iName       = name'
                                , iCapacity   = capacity'
                                , iDurability = durability'
                                , iFlavour    = flavour'
                                , iTexture    = texture'
                                , iCalories   = calories' }


parse' _ = Nothing

data Deliciousness = Delicious Int Int Int Int

instance Monoid Deliciousness where
              mempty = Delicious 0 0 0 0
              Delicious c1 d1 f1 t1 `mappend` Delicious c2 d2 f2 t2 =
                     Delicious (c1 + c2) (d1 + d2)  (f1 + f2) (t1 + t2)

-- Stolen from http://rosettacode.org/wiki/Combinations_with_repetitions#Haskell
combsWithRep :: Int -> [a] -> [[a]]
combsWithRep k xs = combsBySize xs !! k where
       combsBySize = foldr f ([[]] : repeat [])
       f x next = scanl1 (\z n -> map (x:) z ++ n) next

howDelicious :: Deliciousness -> Int
howDelicious (Delicious c d f t) = (max 0 c) * (max 0 d) * (max 0 f) * (max 0 t)

fromIngredient :: Ingredient -> Deliciousness
fromIngredient i = Delicious (iCapacity i) (iDurability i) (iFlavour i) (iTexture i)

deliciousness :: [Ingredient] -> Int
deliciousness ingredients = howDelicious $ foldMap fromIngredient ingredients

type DeliciousRecipe = [(Int, [Ingredient])]

deliciousIngredients :: [Ingredient] -> Int -> DeliciousRecipe
deliciousIngredients ingredients amount =
              let combinations = combsWithRep amount ingredients in
                     zip (map deliciousness combinations) combinations

maxRecipe :: DeliciousRecipe -> Int
maxRecipe recipe = fst $ maximumBy (compare `on` fst) recipe

fiftyCalRecipes :: DeliciousRecipe -> DeliciousRecipe
fiftyCalRecipes = filter ((500==) . (foldr ((+) . iCalories) 0) . snd)


main = do
       contents <- getContents
       let ingredients = mapMaybe parse $ lines contents
       let recipes = deliciousIngredients ingredients 100
       putStrLn $ "part a: " ++ show (maxRecipe recipes)
       let lowCalRecipes = fiftyCalRecipes recipes
       putStrLn $ "part b: " ++ show (maxRecipe lowCalRecipes)

