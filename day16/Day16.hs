{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Day16 (parseSue, newSue, Sue(..), Perhaps(..), compareSues) where

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

type SueLabel = Sue :-> (Maybe Int)

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
setterMap = Map.fromList [ ("children"    , children   )
                         , ("cats"        , cats       )
                         , ("samoyeds"    , samoyeds   )
                         , ("pomeranians" , pomeranians)
                         , ("akitas"      , akitas     )
                         , ("vizslas"     , vizslas    )
                         , ("goldfish"    , goldfish   )
                         , ("trees"       , trees      )
                         , ("cars"        , cars       )
                         , ("perfumes"    , perfumes   )
                         ]

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
        attrLabel' <- Map.lookup attrLabel setterMap
        num' <- readMaybe num
        return $ set attrLabel' (Just num')

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

toTrinary :: Maybe Bool -> Perhaps
toTrinary (Just True)  = Perhaps
toTrinary (Just False) = DefinitelyNot
toTrinary Nothing      = Perhaps

compareAttr :: Sue -> Sue -> SueLabel -> Perhaps
compareAttr s1 s2 sueLabel = toTrinary $ do
     v1 <- get sueLabel s1
     v2 <- get sueLabel s2
     return $ v1 == v2

compareSues :: Sue -> Sue -> Perhaps
compareSues s1 s2 = mconcat $ map (compareAttr s1 s2) (Map.elems setterMap)

findMatchingSue :: Sue -> [Sue] -> Maybe Sue
findMatchingSue s1 = find ((==Perhaps) . compareSues s1)

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
        let matchedSue = findMatchingSue mySue sues
        putStrLn $ maybe "no string found" (show . get number) matchedSue

