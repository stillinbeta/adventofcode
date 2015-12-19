module Day18 (parseGrid, LightGrid(..), Light(..), nextGrid) where

import qualified Data.Sequence as Seq
import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe, catMaybes)
import Debug.Trace

data LightGrid = LightGrid { xSize :: Int
                           , ySize :: Int
                           , getGrid :: !(Seq.Seq (Seq.Seq Light))
                           , stuckOn :: ![(Int, Int)]
                           } deriving Eq
data Light = On | Off deriving Eq

instance Show Light where
        show On  = "#"
        show Off = "."

showRow :: Seq.Seq Light -> String
showRow = foldr ((++) . show) ""

instance Show LightGrid where
        show (LightGrid {getGrid = grid}) = '\n':(foldr ((++) . (++"\n") . showRow) "" grid)

inputGrid = do
        firstLine <- p_firstLine
        -- ensure all lines same length
        let len = length firstLine
        lines <- many (p_otherLines len)
        return (firstLine:lines)

p_firstLine = do
        lights <- many1 p_light
        string "\n"
        return lights

p_otherLines n = do
        lights <- count n p_light
        string "\n"
        return lights

p_light = do
        char <- oneOf "#."
        return $ toLight char

toLight :: Char -> Light
toLight '#' = On
toLight '.' = Off
-- input enforced by parsec

parseGrid :: String -> Either ParseError LightGrid
parseGrid text = do
    grid <- parse inputGrid "(unknown)" text
    let x = length grid
    let y = case grid of
             (row:rest) -> length row
             otherwise -> 0
    let seqGrid = matrixToSeq grid
    return $ LightGrid {xSize = x, ySize = y, getGrid = seqGrid, stuckOn = []}

-- Utilities

matrixToSeq :: [[Light]] -> Seq.Seq (Seq.Seq Light)
matrixToSeq grid = Seq.fromList $ map Seq.fromList grid

mapMatrix :: (Int -> Int -> a -> b) -> Seq.Seq (Seq.Seq a)
                                    -> Seq.Seq (Seq.Seq b)
mapMatrix f mat= Seq.mapWithIndex (\x row ->
                                  Seq.mapWithIndex (f x) row) mat

-- End Utilities --
-- Game step --

indexAllowed :: LightGrid -> Int -> Int -> Bool
indexAllowed lg x y | x >= xSize lg || y >= ySize lg = False
                    | x < 0 || y < 0 = False
                    | otherwise = True

indexMaybe :: LightGrid -> Int -> Int -> Maybe Light
indexMaybe lg x y | indexAllowed lg x y =
                Just $ Seq.index (Seq.index (getGrid lg)  x) y
                  | otherwise = Nothing

lightVal :: LightGrid -> Int -> Int -> Int
lightVal lg x y = fromMaybe 0 $ do
    light <- indexMaybe lg x y
    return $ case light of
                         On -> 1
                         Off -> 0


lightState :: Light -> Int -> Light
lightState On neighboursOn = case neighboursOn of
                                     2 -> On
                                     3 -> On
                                     otherwise -> Off
lightState Off neighboursOn = case neighboursOn of
                                      3 -> On
                                      otherwise -> Off

changeIndex :: LightGrid -> Int -> Int -> Light -> Light
changeIndex lg x y light | elem (x, y) (stuckOn lg) = On
                         | otherwise =
    let neighbours = [lightVal lg (x + x') (y + y') | x' <- [-1..1]
                                                    , y' <- [-1..1]
                                                    , not (x' == 0 && y' == 0)]
        neighbours' = sum neighbours in
            lightState light neighbours'

nextGrid :: LightGrid -> LightGrid
nextGrid lg = let grid' = mapMatrix (changeIndex lg) (getGrid lg) in
                  lg { getGrid = grid' }
-- End game step
-- Begin execution
countOn :: Light -> Int
countOn On  = 1
countOn Off = 0

lightsIlluminated :: LightGrid -> Int
lightsIlluminated LightGrid {getGrid = grid } =
        foldr ((+) . (foldr ((+) . countOn) 0)) 0 grid

stickOn :: LightGrid -> [(Int, Int)] -> LightGrid
stickOn lg@(LightGrid { getGrid = grid }) sticky =
        let sticky' = filter (uncurry (indexAllowed lg)) sticky
            grid' = foldr (\(x, y) grid ->
                        Seq.adjust (Seq.update y On) x grid) grid sticky' in
        lg {stuckOn = sticky', getGrid = grid' }

lightsStuckOn = [(0, 0), (99, 0), (0, 99), (99, 99)]

main = do
    contents <- getContents
    let parsed = parseGrid contents
    case parsed of
        Left err -> putStrLn $ "error: " ++ show err
        Right lg -> do
            let grid100 = (iterate nextGrid lg) !! 100
            putStrLn $ "part a: " ++ (show $ lightsIlluminated grid100)
            let lg' = stickOn lg lightsStuckOn
            let grid100' = (iterate nextGrid lg') !! 100
            putStrLn $ "part b: " ++ (show $ lightsIlluminated grid100')

