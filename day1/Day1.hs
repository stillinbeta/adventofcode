module Day1 ( countParens ) where

countParens :: String -> Int
countParens parens = countParens' parens 0

countParens' :: String -> Int -> Int
countParens' ('(':xs) sum = countParens' xs $ sum + 1
countParens' (')':xs) sum = countParens' xs $ sum - 1
countParens' (_  :xs) sum = countParens' xs sum
countParens' [] sum = sum

main = interact $ show . countParens
