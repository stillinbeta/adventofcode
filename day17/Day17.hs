module Day17 (waysToFill, waysToFillN) where

import Text.Read (readMaybe)
import Data.List (subsequences, lines, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Function (on)

waysToFill :: Int -> [Int] -> [[Int]]
waysToFill capacity cups = atCapacity capacity $ subsequences cups

atCapacity :: Int -> [[Int]] -> [[Int]]
atCapacity capacity cups = filter ((==capacity) . sum) cups

waysToFillN :: Int -> [Int] -> Int -> [[Int]]
waysToFillN capacity cups size = atCapacity capacity $ filter ((==size) . length) $ subsequences cups

smallestSet :: [[Int]] -> Int
smallestSet set = length $ minimumBy (compare `on` length) set

-- Execution and input

toFill = 150

main = do
        contents <- getContents
        let containers = mapMaybe readMaybe $ lines contents
        let filled = waysToFill toFill containers
        putStrLn $ "part a: " ++ (show $ length filled)
        let filledSmall = waysToFillN  toFill containers $ smallestSet filled
        putStrLn $ "part a: " ++ (show $ length filledSmall)
