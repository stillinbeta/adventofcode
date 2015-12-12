module Day1 ( countParens ) where

import Control.Monad
import Control.Monad.State.Lazy

countParens :: String -> (Int, Maybe Int)
countParens parens = runState (countParens' parens 0 1) Nothing

countParens' :: String -> Int -> Int -> State (Maybe Int) Int
countParens' (x:xs) sum loc = do
    let sum' = case x of
                   '(' -> sum + 1
                   ')' -> sum - 1
                   otherwise -> sum
    when (sum' == -1) $ do
        basement <- get
        put (basement `mplus` Just loc)
    countParens' xs sum' (loc + 1)
countParens' [] sum _ = do
    return sum

main = do
    line <- getContents
    let (partA, partB) = countParens line
    putStrLn $ "part a: " ++ show partA
    putStrLn $ "part b: " ++ maybe "never basement" show partB
