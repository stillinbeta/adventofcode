module Day8 (dataLength, stringLex) where

import Data.Char

dataLength :: String -> (Int, Int)
dataLength s = (length s, 0)

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
lex' [] (x:xs)          = [x]:(lex' "" xs)
lex' [] []              = []

main = do
    contents <- getContents
    putStrLn "welp"
