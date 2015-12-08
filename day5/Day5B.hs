module Day5B (isNice) where

import qualified Data.Map.Strict as Map
import Data.List.Split

type PastStrings = Map.Map String Int -- found string -> location

data NameState = NS { pastStrings :: PastStrings
                    , foundDouble :: Bool
                    , foundReapeat:: Bool } deriving Show

instance Monoid NameState where
    mempty = NS Map.empty False False
    -- Note: Map.union will prefer left map in collisions. This is good, we
    -- always want the eldest value
    mappend (NS m1 d1 r1) (NS m2 d2 r2) =
           NS (m1 `Map.union` m2) (d1 || d2) (r1 || r2)

niceCount :: String -> Int
niceCount words = sum $ map (boolToInt . isNice) $ splitOn "\n" words

isNice :: String -> Bool
isNice word = isNice' word mempty 0

isNice' :: String -> NameState -> Int -> Bool
isNice' (c1:c2:c3:cs) ns pos = let NS { pastStrings = ps} = ns
                                   double = isDouble c1 c2 c3
                                   past = isPast c1 c2 pos ps
                                   newMap = pairToMap c1 c2 pos
                                   ns' = NS { pastStrings = newMap
                                            , foundDouble = double
                                            , foundReapeat = past } in
                               isNice' (c2:c3:cs) (ns `mappend` ns') (pos + 1)

isNice' [c1, c2] ns pos = let NS { pastStrings = ps } = ns
                              past = isPast c1 c2 pos ps
                              ns' = mempty { foundReapeat = past } in
                              isNice' [c2] (ns `mappend` ns') (pos + 1)
isNice' _ NS { foundDouble = double, foundReapeat = repeat}  _ = double && repeat

isDouble :: Char -> Char -> Char -> Bool
isDouble c1 _ c3 = c1 == c3

-- x y x y
-- 0 1 2 3w
-- ^   ^
isPast :: Char -> Char -> Int -> PastStrings -> Bool
isPast c1 c2 pos ps = let s = [c1,c2] in
                      case Map.lookup s ps of
                            Just oldpos -> oldpos <= (pos - 2)
                            Nothing -> False

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

pairToMap :: Char -> Char -> Int -> PastStrings
pairToMap c1 c2 pos = Map.singleton [c1,c2] pos

main = interact $ show . niceCount
