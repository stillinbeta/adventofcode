module Day6 (parseLine, Point(Pt), Op(..), Instruction(Instr),
             runInstruction, runInstructions, lightsOn, newGrid, newGrid') where

import Data.List.Split
import Text.Regex.PCRE
import Debug.Trace
import GHC.Exts (Constraint)

class Light a where
       value :: a -> Int
       switch :: Op -> a -> a
       new :: a

newtype LightGrid a =  LG [[a]]

data Point = Pt Int Int deriving (Ord, Eq, Show)
data Op = TurnOn | TurnOff | Toggle deriving (Eq, Show)
data Instruction = Instr Op Point Point deriving (Eq, Show)

data BinaryLight = On | Off
instance Light BinaryLight where
       value On  = 1
       value Off = 0

       switch TurnOn _   = On
       switch TurnOff _  = Off
       switch Toggle On  = Off
       switch Toggle Off  = On

       new = Off

instance Show BinaryLight where
       show On = "*"
       show Off = "."

data DimmerLight = Dimmer Int
instance Light DimmerLight where
       value (Dimmer x) = x

       switch TurnOn  (Dimmer x)  = Dimmer (x + 1)
       switch TurnOff (Dimmer 0)  = Dimmer 0
       switch TurnOff (Dimmer x)  = Dimmer (x - 1)
       switch Toggle  (Dimmer x)  = Dimmer (x + 2)

       new = Dimmer 0

instance Show DimmerLight where
       show (Dimmer x) = show x

newGrid :: LightGrid BinaryLight
newGrid = newGrid' 999

newDimmerGrid :: LightGrid DimmerLight
newDimmerGrid = newGrid' 999

newGrid' :: (Light a) => Int -> LightGrid a
newGrid' dim = LG [ [new | _ <- [0..dim]] | _ <- [0..dim] ]

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
-- Beginning of processing

runInstructions :: (Light a) => [Instruction] -> LightGrid a -> LightGrid a
runInstructions (i:is) lg = runInstructions is $ runInstruction i lg
runInstructions [] lg = lg

runInstruction :: (Light a) => Instruction -> LightGrid a -> LightGrid a
runInstruction (Instr op pt1 pt2) (LG grid) =
       LG $ walkColumn pt1 pt2 0 (switch op) grid

walkColumn :: Point -> Point -> Int -> (a -> a) -> [[a]] -> [[a]]
walkColumn pt1 pt2 pos f (c:cs) =
       let (Pt x1 y1) = pt1
           (Pt x2 y2) = pt2
           c' = if pos >= x1 && pos <= x2
                then walkRow y1 y2 0 f c
                else c in
       c':walkColumn pt1 pt2 (pos + 1) f cs

walkColumn _ _ _ _ [] = []

walkRow :: Int -> Int -> Int -> (a -> a) -> [a] -> [a]
walkRow start end pos f (x:xs) =
       let x' = if pos >= start && pos <= end
                then f x
                else x in
           --x'' = trace ("x' = " ++ show x' ++ "@" ++ show start ++"," ++ show end ++":" ++ show pos ++ "\n") x' in
       x':(walkRow start end (pos + 1) f xs)
walkRow _ _ _ _ [] = []

lightsOn :: (Light a) => LightGrid a -> Int
lightsOn (LG grid) = sum $ map lightsOn' grid

lightsOn' :: (Light a) => [a] -> Int
lightsOn' row = sum $ map value row

-- Begin of Printing
printGrid :: (Show a) => LightGrid a -> String
printGrid (LG grid) = foldl1 (++) $  map printRow grid

printRow :: (Show a) => [a] -> String
printRow row = (foldl1 (++) (map show row)) ++ "\n"

instance (Show a) => Show (LightGrid a) where
       show = printGrid

-- End of Printing

-- End of processing
-- Beginnning of execution

getInstructions :: String -> [Instruction]
getInstructions input =
       let lines = takeWhile (/="") $ splitOn "\n" input in
       map parseLine lines

runGrid :: (Light a) => LightGrid a -> [Instruction] -> Int
runGrid grid instructions = lightsOn $ runInstructions instructions grid

runBinary :: [Instruction] -> Int
runBinary  = runGrid newGrid

runDimmer :: [Instruction] -> Int
runDimmer = runGrid newDimmerGrid

main = interact $ show . runDimmer . getInstructions
-- main = interact $ show . runBinary . getInstructions

-- End of execution
