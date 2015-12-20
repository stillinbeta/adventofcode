module Day19 (parseCalibration, substitutionCount, searchDepth) where

import Control.Monad.State.Lazy (State(..), execState, get, modify)
import qualified Data.Set as Set
import Data.Maybe (maybeToList, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec hiding (State)

import Debug.Trace

import Data.Char (isLower)

type Calibration = Map.Map String [String]

p_input = do
        lines <- endBy1 p_calibration newline
        newline
        formula  <- many1 letter
        return (formula, foldr (\(k, v) m ->
            Map.insertWith (++) k [v] m) Map.empty lines)

p_calibration = do
        alias <- try p_chemical
        string " => "
        value <- p_chemicals
        return (alias, value)

p_chemical = try (string "e") <|> do
        letter <- upper
        letter2 <- try (optionMaybe lower)
        return $ letter:(maybeToList letter2)

p_chemicals = do
        chemicals <- many1 p_chemical
        return $ concat chemicals

p_chemicalPlus = do
        chemical <- p_chemical
        rest <- many letter
        return (chemical, rest)

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

commonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
commonPrefix (x1:xs1) (x2:xs2) | x1 /= x2 = ([], x1:xs1, x2:xs2)
                               | x1 == x2 =
                       let (cs1, xs1', xs2') = commonPrefix xs1 xs2 in
                           (x1:cs1, xs1', xs2')
commonPrefix [] xs2 = ([], [], xs2)
commonPrefix xs1 [] = ([], xs1, [])

commonPrefix1 :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
commonPrefix1 (x1:xs1) (x2:xs2) | x1 /= x2 = ([], x1:xs1, x2:xs2)
                                | x1 == x2 = ([x1], xs1, xs2)
commonPrefix1 [] xs2 = ([], [], xs2)
commonPrefix1 xs1 [] = ([], xs1, [])

getChemical :: String -> Maybe (String, String)
getChemical string = case parse p_chemicalPlus "" string of
                         Left _ -> Nothing
                         Right res -> Just res

searchDepth :: String -> Calibration -> Maybe Int
searchDepth query calibration = searchDepth' 0 query calibration "e"

searchDepth' :: Int -> String -> Calibration -> String -> Maybe Int
searchDepth' depth query calibration curr | length query < length curr = Nothing
                                          | otherwise =
          case traceShow (curr, query) $ traceShowId $ commonPrefix1 query curr of
                  (_, [], []) -> Just depth
                  (_, query', curr') -> do
                      (chem, rest) <- getChemical curr'
                      subts <- Map.lookup chem calibration
                      let f = (searchDepth' (depth + 1) query' calibration) . (++rest)
                      listToMaybe $ mapMaybe f subts
main = do
    contents <- getContents
    case parseCalibration contents of
        Left err -> putStrLn $ "error: " ++ show err
        Right (formula, calibration) -> do
            let substCount = substitutionCount formula calibration
            putStrLn $ "part a: " ++ show substCount
            let depth = searchDepth formula calibration
            putStrLn $ "part b: " ++ (maybe "no result" show depth)
