module Day6 (parseLine, Point(Pt), Op(..), Instruction(Instr),
             runInstruction, lightsOn, newGrid, newGrid') where

import Data.List.Split
import Text.Regex.PCRE
import Debug.Trace

newtype LightGrid = LG [[Bool]]

data Point = Pt Int Int deriving (Ord, Eq, Show)
data Op = TurnOn | TurnOff | Toggle deriving (Eq, Show)
data Instruction = Instr Op Point Point deriving (Eq, Show)

instance Show LightGrid where
       show = printGrid

newGrid :: LightGrid
newGrid = newGrid' 999

newGrid' :: Int -> LightGrid
newGrid' dim = LG [ [False | _ <- [0..dim]] | _ <- [0..dim] ]
-- Beginning of Parsing
pattern = "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)"

parseLine :: String -> Instruction
parseLine line = case line =~ pattern :: (String, String, String, [String]) of
         (_, _, _, [instr, x1, y1, x2, y2]) ->
              Instr (parseOp instr) (makePoint x1 y1) (makePoint x2 y2)
         otherwise -> error $ "Couldn't parse \"" ++ line ++ "\"\n"

parseOp :: String -> Op
parseOp("turn on")  = TurnOn
parseOp("turn off") = TurnOff
parseOp("toggle")   = Toggle

makePoint :: String -> String -> Point
makePoint x y = Pt (read x) (read y)

-- End of parsing
-- Beginningl of processing

runInstruction :: Instruction -> LightGrid -> LightGrid
runInstruction (Instr op pt1 pt2) grid =
       let f = case op of
                     TurnOn -> turnOn
                     TurnOff -> turnOff
                     Toggle -> toggle in
       walkColumn pt1 pt2 0 f grid

turnOn :: Bool -> Bool
turnOn _ = True

turnOff :: Bool -> Bool
turnOff _ = False

toggle :: Bool -> Bool
toggle x | trace ("toggling" ++ show x ++ "\n") False = undefined
toggle True = False
toggle False = True

walkColumn :: Point -> Point -> Int -> (Bool -> Bool) -> LightGrid -> LightGrid
walkColumn pt1 pt2 pos f (LG (c:cs)) =
       let (Pt x1 y1) = pt1
           (Pt x2 y2) = pt2
           c' = if pos >= x1 && pos <= x2
                then walkRow y1 y2 0 f c
                else c
           (LG lg) = walkColumn pt1 pt2 (pos + 1) f (LG cs) in
       LG (c':lg)
walkColumn _ _ _ _ (LG []) = LG []

walkRow :: Int -> Int -> Int -> (Bool -> Bool) -> [Bool] -> [Bool]
walkRow start end pos f (x:xs) =
       let x' = if pos >= start && pos <= end
                then trace "calling f\n"(f x)
                else x
           x'' = trace ("x' = " ++ show x' ++ "@" ++ show start ++"," ++ show end ++":" ++ show pos ++ "\n") x' in
       x'':(walkRow start end (pos + 1) f xs)
walkRow _ _ _ _ [] = []


printGrid :: LightGrid -> String
printGrid (LG grid) = foldl (++) "" $ map printRow grid

printRow :: [Bool] -> String
printRow row = (map printCell row) ++ "\n"

printCell :: Bool -> Char
printCell True = '*'
printCell False = '.'


lightsOn :: LightGrid -> Int
lightsOn (LG grid) = foldl lightsOn' 0 grid

lightsOn' :: Int -> [Bool] -> Int
lightsOn' initial row = foldl lightsOn'' initial row

lightsOn'' :: Int -> Bool -> Int
lightsOn'' x True = x + 1
lightsOn'' x False = x

-- End of processing
-- Beginnning of execution


getLights :: String -> Int
getLights input =  let lines = takeWhile (/="") $ splitOn "\n" input
                       instructions = map parseLine lines
                       result = foldr runInstruction newGrid instructions in
                   lightsOn result

main = interact $ show . getLights

-- End of execution
