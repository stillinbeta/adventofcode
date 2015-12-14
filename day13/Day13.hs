module Day13 ( parse, Happiness(Happiness), findOptimal ) where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Read

type Name = String
data Happiness = Happiness Name Name Int deriving (Eq, Show)
type HappinessMap = Map.Map (Name, Name) Int

parse :: String -> Maybe Happiness
parse = parse' . words

parse' :: [String] -> Maybe Happiness
parse' [ name1, "would", direction, hunits
       , "happiness", "units", "by", "sitting"
       , "next", "to", name2] = do
             sign <- case direction of
                            "gain" -> Just 1
                            "lose" -> Just (-1)
                            otherwise -> Nothing
             hunits' <- readMaybe hunits
             let name2' = init name2 -- Sentence ends with a full stop
             return $ Happiness name1 name2' (sign * hunits')
parse' _ = Nothing



allNames :: [Happiness] -> Set.Set Name
allNames ((Happiness n1 n2 hunits):rest) =
       Set.insert n1 $ Set.insert n2 $ allNames rest
allNames [] = Set.empty

createMap :: [Happiness] -> HappinessMap
createMap ((Happiness n1 n2 hunits):hs) =
       Map.insert (n1, n2) hunits $ createMap hs
createMap [] = Map.empty

netHappiness :: HappinessMap -> [Name] -> Maybe Int
-- Loop around the head and the tail
netHappiness hmap names = netHappiness' hmap $ names ++ [head names]

netHappiness' :: HappinessMap -> [Name] -> Maybe Int
netHappiness' hmap (n1:n2:rest) = do
       diff1 <- Map.lookup (n1, n2) hmap
       diff2 <- Map.lookup (n2, n1) hmap
       rest <- netHappiness' hmap (n2:rest)
       return (rest + diff1 + diff2)

netHappiness' _ _ = Just 0

addMe :: HappinessMap -> [Name] -> (HappinessMap, [Name])
addMe hmap names =
              let hmap' = foldr (\name acc ->
                                Map.insert (name, "me") 0 $ Map.insert ("me", name) 0 acc) hmap names in
              (hmap', "me":names)

findOptimal :: [Happiness] -> Int
findOptimal happiness =
       let hmap = createMap happiness
           names = Set.toList $ allNames happiness
           permutes = permutations names in
       maximum $ catMaybes $ map (netHappiness hmap) permutes

findOptimalWithMe :: [Happiness] -> Int
findOptimalWithMe happiness =
       let hmap = createMap happiness
           names = Set.toList $ allNames happiness
           (hmap', names') = addMe hmap names
           permutes = permutations names' in
       maximum $ catMaybes $ map (netHappiness hmap') permutes

main = do
       contents <- getContents
       let happiness = catMaybes $ map parse (lines contents)
       putStrLn $ "part a: " ++ show (findOptimal happiness)
       putStrLn $ "part b: " ++ show (findOptimalWithMe happiness)
