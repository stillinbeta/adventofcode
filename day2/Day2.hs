module Day2 ( presentArea, allPresentArea ) where

import Data.List.Split

data Present = Present Int Int Int

presentArea :: String -> Int
presentArea = presentArea' . presentParser

presentParser :: String -> Present
presentParser present = case splitOn "x" present of
                            [l, w, h] -> Present (read l) (read w) (read h)
                            _ -> Present 0 0 0 -- trailing newline
                            -- _ -> error $ "can't parse" ++ present

presentArea' :: Present -> Int
presentArea' (Present l w h) = let prod1 = l * w
                                   prod2 = w * h
                                   prod3 = l * h
                               in minimum [prod1, prod2, prod3] + 2 * (prod1 + prod2 + prod3)

allPresentArea :: String -> Int
allPresentArea presents = sum ( map presentArea $ splitOn "\n" presents )

main = interact $ show . allPresentArea
