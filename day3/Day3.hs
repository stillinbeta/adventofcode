module Day3 (santaRun) where

import qualified Data.Set as Set

data Point = Pt Int Int deriving (Eq, Ord)

data SantaState = SantaState (Set.Set Point) Point


baseSantaState = SantaState (Set.singleton pt) pt where
                 pt = Pt 0 0

santaRun :: String -> Int
santaRun run = runLength $ foldl santaMove baseSantaState run

runLength :: SantaState -> Int
runLength (SantaState set _) = Set.size set

santaMove :: SantaState -> Char -> SantaState
santaMove (SantaState set pos) chr = let pos' = move pos chr in
                                     SantaState (Set.insert pos' set) pos'

move :: Point -> Char -> Point
move (Pt x y) '>' = Pt (x+1) y
move (Pt x y) '<' = Pt (x-1) y
move (Pt x y) '^' = Pt x (y+1)
move (Pt x y) 'v' = Pt x (y-1)
move point _ = point

main = interact $ show . santaRun
