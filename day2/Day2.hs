module Day2 (presentParser, presentArea, ribbonLength , Present(Present)) where

import Data.List
import Data.List.Split
import Data.Maybe

data Present = Present Int Int Int deriving Show

presentParser :: String -> Maybe Present
presentParser present = case splitOn "x" present of
                            [l, w, h] -> Just $ Present (read l) (read w) (read h)
                            _ -> Nothing

presentArea :: Present -> Int
presentArea (Present l w h) = let prod1 = l * w
                                  prod2 = w * h
                                  prod3 = l * h in
                               minimum [prod1, prod2, prod3] + 2 * (prod1 + prod2 + prod3)

ribbonLength :: Present -> Int
ribbonLength (Present l w h) = let (d1:d2:_) = sort [l, w, h] in
                            2 * (d1 + d2) + l * w * h

main = do
    contents <- getContents
    let presents = catMaybes $ map presentParser $ lines contents
    let allPresentArea = foldr ((+) . presentArea) 0 presents
    putStrLn $ "part a: " ++ show allPresentArea
    let allRibbonLength = foldr ((+) . ribbonLength) 0 presents
    putStrLn $ "part b: " ++ show allRibbonLength
