{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Day16 (parseSue, newSue, Sue(..), Perhaps(..),
              compareSues, compareAttr, compareAttrB) where

import Control.Category
import Data.Either (rights)
import Data.List (find, lines)
import Data.Label
import Data.Maybe (catMaybes, maybe)
import qualified Data.Map.Strict as Map
import Prelude hiding ((.), id)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

-- Begin Data Model --

-- We can't match on functions unfortunately
data SueLabel = SueLabel { labelName  :: String
                         ,  runLabel  :: Sue :-> (Maybe Int)
                         }

data Sue  = Sue { _children    :: Maybe Int
                , _cats        :: Maybe Int
                , _samoyeds    :: Maybe Int
                , _pomeranians :: Maybe Int
                , _akitas      :: Maybe Int
                , _vizslas     :: Maybe Int
                , _goldfish    :: Maybe Int
                , _trees       :: Maybe Int
                , _cars        :: Maybe Int
                , _perfumes    :: Maybe Int
                , _number :: Int
                } deriving (Eq, Show)

mkLabel ''Sue

newSue num = Sue { _children    = Nothing
                 , _cats        = Nothing
                 , _samoyeds    = Nothing
                 , _pomeranians = Nothing
                 , _akitas      = Nothing
                 , _vizslas     = Nothing
                 , _goldfish    = Nothing
                 , _trees       = Nothing
                 , _cars        = Nothing
                 , _perfumes    = Nothing
                 , _number = num
                 }


setterMap :: Map.Map String SueLabel
setterMap = let list =  [ SueLabel "children"    children
                        , SueLabel "cats"        cats
                        , SueLabel "samoyeds"    samoyeds
                        , SueLabel "pomeranians" pomeranians
                        , SueLabel "akitas"      akitas
                        , SueLabel "vizslas"     vizslas
                        , SueLabel "goldfish"    goldfish
                        , SueLabel "trees"       trees
                        , SueLabel "cars"        cars
                        , SueLabel "perfumes"    perfumes
                        ] in
                            Map.fromList $ map (\sl -> (labelName sl, sl)) list
-- End Data Model --
-- Begin Parsing --

sueLine = do
    num <- sueNum
    string ": "
    cells <- sepBy attr (string ", ")
    return $ foldr ($) (newSue num) (catMaybes cells)

sueNum = do
    string "Sue "
    num <- many digit
    return $ read num

attr = do
    attrLabel <- possibleLabels
    string ": "
    num <- many digit
    return $ getSetter attrLabel num

possibleLabels = choice $ map (try . string) (Map.keys setterMap)

getSetter :: String -> String -> Maybe (Sue -> Sue)
getSetter attrLabel num = do
        sueLabel <- Map.lookup attrLabel setterMap
        num' <- readMaybe num
        return $ set (runLabel sueLabel) (Just num')

parseSue :: String -> Either ParseError Sue
parseSue text = parse sueLine "(unknown)" text

-- End Parsing
-- Begin Matching

-- Just bool, but easier to reason about
data Perhaps = Perhaps | DefinitelyNot deriving (Show, Eq)

instance Monoid Perhaps where
        mempty = Perhaps

        DefinitelyNot `mappend` _ = DefinitelyNot
        Perhaps       `mappend` t = t

toPerhaps :: Maybe Bool -> Perhaps
toPerhaps (Just True)  = Perhaps
toPerhaps (Just False) = DefinitelyNot
toPerhaps Nothing      = Perhaps

type CompareF = Sue -> Sue -> SueLabel -> Perhaps

compareAttr :: CompareF
compareAttr = compareAttr' (==)

compareAttr' :: (Int -> Int -> Bool) -> Sue -> Sue -> SueLabel -> Perhaps
compareAttr' f s1 s2 sueLabel = toPerhaps $ do
     v1 <- get (runLabel sueLabel) s1
     v2 <- get (runLabel sueLabel) s2
     return $ v1 `f` v2

compareAttrB :: CompareF
compareAttrB s1 s2 sueLabel =
        let f = case labelName sueLabel of
                    "cats"        -> (<)
                    "trees"       -> (<)
                    "pomeranians" -> (>)
                    "goldfish"    -> (>)
                    otherwise   -> (==) in
                        compareAttr' f s1 s2 sueLabel

compareSues :: (CompareF) -> Sue -> Sue -> Perhaps
compareSues f s1 s2 = mconcat $ map (f s1 s2) (Map.elems setterMap)

-- End matching --
-- Begin execution --

findMatchingSueA :: Sue -> [Sue] -> Maybe Sue
findMatchingSueA = findMatchingSue' compareAttr

findMatchingSueB :: Sue -> [Sue] -> Maybe Sue
findMatchingSueB = findMatchingSue' compareAttrB

findMatchingSue' :: (CompareF) -> Sue -> [Sue] -> Maybe Sue
findMatchingSue' f s1 = find ((==Perhaps) . compareSues f s1)
-- End matching --
-- Begin execution --

mySue = Sue { _children    = Just 3
            , _cats        = Just 7
            , _samoyeds    = Just 2
            , _pomeranians = Just 3
            , _akitas      = Just 0
            , _vizslas     = Just 0
            , _goldfish    = Just 5
            , _trees       = Just 3
            , _cars        = Just 2
            , _perfumes    = Just 1
            , _number = 0
            }
main = do
        contents <- getContents
        let sues = rights $ map parseSue (lines contents)
        let matchedSueA = findMatchingSueA mySue sues
        putStrLn $ "part a: " ++ maybe "no string found" (show . get number) matchedSueA
        let matchedSueB = findMatchingSueB mySue sues
        putStrLn $ "part b: " ++ maybe "no string found" (show . get number) matchedSueB
-- End execution --
