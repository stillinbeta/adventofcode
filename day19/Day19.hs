module Day19 (parseCalibration, substitutionCount, searchDepth) where

import Control.Monad.State.Lazy (State(..), execState, get, modify)
import qualified Data.Set as Set
import Data.Maybe (maybeToList, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec hiding (State)

import Data.Char (isLower)

type Calibration = Map.Map String [String]

p_input = do
        lines <- endBy1 p_calibration newline
        newline
        formula  <- many1 letter
        return (formula, foldr (\(k, v) m ->
            Map.insertWith (++) k [v] m) Map.empty lines)

p_calibration = do
        alias <- try (p_chemical) <|> string "e"
        string " => "
        value <- p_chemicals
        return (alias, value)

p_chemical = do
        letter <- upper
        letter2 <- try (optionMaybe lower)
        return $ letter:(maybeToList letter2)

p_chemicals = do
        chemicals <- many1 p_chemical
        return $ concat chemicals

parseCalibration :: String -> Either ParseError (String, Calibration)
parseCalibration = parse p_input "(unknown)"

substitutionCount :: String -> Calibration -> Int
substitutionCount formula calibration =
        let subst = substitutionCount' "" formula calibration in
            Set.size $ execState subst Set.empty

substitutionCount' :: String -> String -> Calibration -> State (Set.Set String) ()
substitutionCount' _ [] _ = return ()

substitutionCount' prev xs calibration = do
        let (key, next) = case xs of
                      (x1:x2:xs) -> if isLower x2
                                        then (x1:x2:[], xs)
                                        else (x1:[], x2:xs)
                      [x1]       -> ([x1], [])
        let lookup = Map.lookup key calibration
        case lookup of
            Just subs -> modify $ (flip (foldr Set.insert)) $ map ((prev++) . (++next)) subs
            Nothing -> pure ()
        substitutionCount' (prev ++ key) next calibration

searchDepth :: String -> Calibration -> Maybe Int
searchDepth query calibration = searchDepth' 0 query calibration "e"

searchDepth' :: Int -> String -> Calibration -> String -> Maybe Int
searchDepth' 30 _ _ _ = Nothing
searchDepth' depth query calibration curr | query == curr = Just depth
                                          | otherwise =
          let set = execState (substitutionCount' "" curr calibration) Set.empty
              words = Set.toList set in
                  listToMaybe $ mapMaybe (searchDepth' (depth + 1) query calibration) words


main = do
    contents <- getContents
    case parseCalibration contents of
        Left err -> putStrLn $ "error: " ++ show err
        Right (formula, calibration) -> do
            let substCount = substitutionCount formula calibration
            putStrLn $ "part a: " ++ show substCount
            let depth = searchDepth formula calibration
            putStrLn $ "part b: " ++ (maybe "no result" show depth)
