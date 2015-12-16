module Day14 ( parse, Reindeer(..), runRunReindeer ) where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Text.Read as Read
import Data.Maybe
import Data.List

data Reindeer = Reindeer { velocity :: Int
                         , sprint :: Int
                         , rest :: Int } deriving (Eq, Show)

data ReindeerStatus = Sprinting | Resting deriving Show
data ReindeerState = RS { getReindeer :: Reindeer
                        , rStatus  :: ReindeerStatus
                        , rTimeIn  :: Int } deriving Show


parse :: String -> Maybe Reindeer
parse = parse' . words

parse' :: [String] -> Maybe Reindeer
parse' [_, "can", "fly", v, "km/s", "for", s, "seconds,"
       , "but", "then", "must", "rest", "for", r, "seconds."] = do
              v' <- Read.readMaybe v
              s' <- Read.readMaybe s
              r' <- Read.readMaybe r
              return $ Reindeer v' s' r'
parse' _ = Nothing

makeState :: Reindeer -> ReindeerState
makeState reindeer = (RS reindeer Sprinting 1)

runRunReindeer :: Int -> Reindeer -> Int
runRunReindeer count reindeer = sum $ runRunReindeer' count reindeer

runRunReindeer' :: Int -> Reindeer -> [Int]
runRunReindeer' count reindeer = evalState (replicateM count reindeerTick) (makeState reindeer)

toggleState :: ReindeerState -> ReindeerState
toggleState (RS reindeer Sprinting _) = (RS reindeer Resting 1)
toggleState (RS reindeer Resting _) =   (RS reindeer Sprinting 1)

incrState state@RS { rTimeIn = timeIn} = state { rTimeIn = timeIn + 1 }

reindeerTick :: State ReindeerState Int
reindeerTick = do
              state <- get
              let reindeer = getReindeer state
              case rStatus state of
                     Sprinting -> do
                            if (rTimeIn state >= sprint reindeer)
                                   then (modify toggleState)
                                   else (modify incrState)
                            return $ velocity reindeer
                     Resting -> do
                            if (rTimeIn state >= rest reindeer)
                                   then (modify toggleState)
                                   else (modify incrState)
                            return 0

scoreMax :: Int -> Int -> Int
scoreMax max i = if i >= max then 1
                             else 0


runningTally :: [Int] -> [Int]
runningTally = snd . mapAccumL (\a b -> (a + b, a + b)) 0

runAllReindeer :: Int -> [Reindeer] -> [Int]
runAllReindeer c reindeer =
                  -- [[7, 7, 0, 0], [6, 6, 6, 0]]
              let distances = map (runRunReindeer' c) reindeer
                  -- [[7, 6], [14, 12], [14, 18], [14, 18]]
                  distances' = transpose $ map runningTally distances in
                         scoreDistances distances'


scoreDistances :: [[Int]] -> [Int]
scoreDistances (d:ds) =
              let max = maximum d
                  scores = map (scoreMax max) d in
                         case ds of
                                [] -> scores
                                otherwise -> zipWith (+) scores $ scoreDistances ds
main = do
       let seconds = 2503
       contents <-  getContents
       let reindeer = mapMaybe parse $ lines contents
       let fastest = maximum $ map (runRunReindeer seconds) reindeer
       putStrLn $ "part a: " ++ show fastest
       let highestScoring = maximum $ runAllReindeer seconds reindeer
       putStrLn $ "part b: " ++ show highestScoring

