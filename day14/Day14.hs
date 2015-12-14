module Day14 ( parse, Reindeer(..), runRunReindeer ) where

import Text.Read
import Data.Maybe

data Reindeer = Reindeer { velocity :: Int
                         , sprint :: Int
                         , rest :: Int } deriving (Eq, Show)

data ReindeerState = Sprinting | Resting

parse :: String -> Maybe Reindeer
parse = parse' . words

parse' :: [String] -> Maybe Reindeer
parse' [_, "can", "fly", v, "km/s", "for", s, "seconds,"
       , "but", "then", "must", "rest", "for", r, "seconds."] = do
              v' <- readMaybe v
              s' <- readMaybe s
              r' <- readMaybe r
              return $ Reindeer v' s' r'
parse' _ = Nothing

runRunReindeer :: Int -> Reindeer -> Int
runRunReindeer secs reindeer = runRunReindeer' secs reindeer Sprinting 1

runRunReindeer' :: Int -> Reindeer  -> ReindeerState -> Int -> Int
runRunReindeer' 0 _ _ _ = 0
runRunReindeer' timeRemaining reindeer Sprinting timeInState
       | timeInState == sprint reindeer = distance + f Resting 1
       | otherwise = distance + f Sprinting (timeInState + 1)
       where distance = velocity reindeer
             f = runRunReindeer' (timeRemaining - 1)reindeer
runRunReindeer' timeRemaining reindeer Resting timeInState
       | timeInState == rest reindeer = f Sprinting 1
       | otherwise = f Resting (timeInState + 1)
       where f = runRunReindeer' (timeRemaining - 1) reindeer

main = do
       let seconds = 2503
       contents <-  getContents
       let reindeer = mapMaybe parse $ lines contents
       let fastest = maximum $ map (runRunReindeer seconds) reindeer
       putStrLn $ "part a: " ++ show fastest

