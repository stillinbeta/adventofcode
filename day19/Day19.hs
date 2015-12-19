module Day19 (parseCalibration, substitutionCount) where

import Control.Monad.State.Lazy (State(..), evalState, get, modify)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)
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
            evalState subst Set.empty

substitutionCount' :: String -> String -> Calibration -> State (Set.Set String) Int
substitutionCount' _ [] _ = do
        set <- get
        return $ Set.size set

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

main = do
    contents <- getContents
    case parseCalibration contents of
        Left err -> putStrLn $ "error: " ++ show err
        Right (formula, calibration) -> do
            let substCount = substitutionCount formula calibration
            putStrLn $ "part a: " ++ show substCount
