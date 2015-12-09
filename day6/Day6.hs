--module Day6 (parseLine, Point(Pt), Op(..), Instruction(Instr),
--             runLine, lightsOn, newPointSet) where

import Data.Hashable
import qualified Data.HashSet as Set
import Data.List.Split
import Text.Regex.PCRE
import Debug.Trace

data Point = Pt Int Int deriving (Ord, Eq, Show)
data Op = TurnOn | TurnOff | Toggle deriving (Eq, Show)
data Instruction = Instr Op Point Point deriving (Eq, Show)

instance Hashable Point where
       hash (Pt a1 a2) = hash a1 `hashWithSalt` a2
       hashWithSalt s (Pt a1 a2) = s `hashWithSalt` a1 `hashWithSalt` a2

type PointSet = Set.Set Point

newPointSet :: PointSet
newPointSet = Set.empty
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

runLine :: Instruction -> PointSet -> PointSet
runLine (Instr opt (Pt x1 y1)  (Pt x2 y2)) ps =
       let fun = case opt of
              TurnOn  -> Set.insert
              TurnOff -> Set.delete
              Toggle  -> toggle
           worklist = [Pt x y | x <- [x1..x2], y <- [y1..y2]] in
           foldr fun ps worklist

toggle :: Point -> PointSet -> PointSet
toggle var set = if Set.member var set
                 then Set.delete var set
                 else Set.insert var set

lightsOn :: PointSet -> Int
lightsOn = Set.size
-- End of processing
-- Beginnning of execution


getLights :: String -> Int
getLights input =  let lines = takeWhile (/="") $ splitOn "\n" input
                       lines' = lines -- trace ("lines = " ++ show lines) lines
                       instructions = map parseLine lines'
                       i' = instructions -- trace ("instructions = " ++ show instructions) instructions
                       result = foldr runLine newPointSet i' in
                   lightsOn result

main = interact $ show . getLights

-- End of execution
