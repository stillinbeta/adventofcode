module Day5 (isNice) where

import Data.List.Split

data WordState = WS { vowels           :: Int,
                      doubleLetter     :: Bool,
                      forbiddenStrings :: Bool }


instance Monoid WordState where
    mempty = WS 0 False False
    mappend (WS v1 dl1 fs1) (WS v2 dl2 fs2) = WS (v1 + v2) (dl1 || dl2) (fs1 || fs2)

niceCount :: String -> Int
niceCount words = sum $ map (boolToInt . isNice) $ splitOn "\n" words

isNice :: String -> Bool
isNice word = isNice' word mempty

isNice' :: String -> WordState -> Bool
isNice' (c1:c2:cs) ws = let vc = vowelCount c1
                            dl = isDouble c1 c2
                            f = isForbidden c1 c2 in
                        isNice' (c2:cs) $ mappend ws $ WS vc dl f

isNice' [c] ws = isNice' [] $ mappend ws $ mempty { vowels = vowelCount c }
isNice' [] WS { vowels = v, doubleLetter = dl, forbiddenStrings = fs} =
    v >= 3 &&  dl && not fs

vowelCount :: Char -> Int
vowelCount = boolToInt . isVowel

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

isDouble :: Char -> Char -> Bool
isDouble = (==)

isForbidden :: Char -> Char -> Bool
isForbidden 'a' 'b' = True
isForbidden 'c' 'd' = True
isForbidden 'p' 'q' = True
isForbidden 'x' 'y' = True
isForbidden _ _ = False

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

main = interact $ show . niceCount
