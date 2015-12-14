module Day9 ( routeLengths
            , Distance(Distance)
            , Pair(Pair)
            , parse) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Read

type City = String

data Pair = Pair City City deriving Show
data Distance = Distance Pair Int deriving (Eq, Show)

type DistanceList = [(Pair, Int)]

instance Eq Pair where
        (Pair c1a c1b) == (Pair c2a c2b) =
            (c1a == c2a) && (c1b == c2b) ||
            (c1a == c2b) && (c1b == c2a)

loadDistance :: [Distance] -> DistanceList
loadDistance = map (\(Distance pair dist) -> (pair, dist))


getCities :: [Distance] -> Set.Set City
getCities ((Distance (Pair c1 c2) _):ds) =
        let set = Set.insert c1 $ getCities ds in
            Set.insert c2 set
getCities [] = Set.empty

distance :: DistanceList -> [City] -> Maybe Int
distance dl (c1:c2:cs) = do
        dist <- lookup (Pair c1 c2) dl
        sum <- distance dl (c2:cs)
        return $ dist + sum
distance _ m = Just 0

routeLengths :: [Distance] -> [Int]
routeLengths distances =
        let cities = getCities distances
            distanceList = loadDistance distances
            allRoutes = permutations $ Set.toList cities in
                catMaybes $ map (distance distanceList) allRoutes

-- Stolen from Network.CGI.Protocol
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

parse :: String -> Maybe Distance
parse = parse' . words

parse' :: [String] -> Maybe Distance
parse' [city1, "to", city2,  "=", dist] = do
        dist' <- readMaybe dist
        return $ Distance (Pair city1 city2) dist'
parse' _ = Nothing

main = do
    contents <- getContents
    let distances = catMaybes $ map parse (lines contents)
    let lengths = routeLengths distances
    putStrLn $ "part a: " ++ show ( minimum lengths)
    putStrLn $ "part b: " ++ show (maximum lengths)
