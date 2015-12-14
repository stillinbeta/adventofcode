module Day8 (dataLength, stringLex, CodeDiff(CodeDiff), escape) where

import Data.Char

data CodeDiff = CodeDiff Int Int deriving (Eq, Show)

instance Monoid CodeDiff where
        mempty = CodeDiff 0 0
        (CodeDiff s1 b1) `mappend` (CodeDiff s2 b2) = CodeDiff (s1 + s2) (b1 + b2)

dataLength :: String -> CodeDiff
dataLength s = CodeDiff (length s) (length (stringLex s) - 2)

stringLex :: String -> [String]
stringLex = lex' ""

lex' :: String -> String -> [String]
lex' buf@('\\':'x':rest) (x:xs) | isHexDigit x =
        case rest of
            [] ->         lex' (buf ++ [x]) xs
            otherwise -> (buf ++ [x]):(lex' "" xs)
lex' buf@('\\':'x':rest) (x:xs) | not $ isHexDigit x =
        let buf' = case rest of
                       [] -> ["\\\\", "x"]
                       -- Should be a single char in an array
                       otherwise -> ["\\\\", "x", rest] in
        buf' ++ (lex' "" (x:xs))
lex' "\\" ('\\':xs)    = "\\\\":(lex' "" xs)
lex' "\\" ('x':xs)     = lex' ("\\" ++ "x") xs
lex' "" ('\\':xs)      = lex' "\\" xs
-- Somehow we've got a buffer but it matches nothing (likely nonhex after
-- \x
lex' _ (x:xs)          = [x]:(lex' "" xs)
lex' [] []              = []

escape :: String -> String
escape input = "\"" ++ escape' input ++ "\""

escape' ('\"':xs) = ('\\':'"':escape' xs)
escape' ('\\':xs) = ('\\':'\\':escape' xs)
escape' (x:xs)    = (x:escape' xs)
escape' []        = []

main = do
    contents <- getContents
    let strings = lines contents
    let (CodeDiff slength bytes) = foldMap dataLength strings
    putStrLn $ "part a: " ++ show (slength - bytes)
    let escaped = foldr ((+) . length . escape) 0 strings
    putStrLn $ "part b: " ++ show (escaped - length)
