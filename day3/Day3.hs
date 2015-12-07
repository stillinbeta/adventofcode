module Day3 (santaRun) where

import Text.Printf
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Point = Pt Int Int deriving (Eq, Ord)

data SantaState = SantaState (Set.Set Point) (Seq.Seq Point) Int

baseSantaState :: Int -> SantaState
baseSantaState ct = SantaState (Set.singleton pt) (Seq.replicate ct pt) 0 where
                    pt = Pt 0 0

santaRun :: Int -> String -> Int
santaRun santas run  = runLength $ foldl santaMove (baseSantaState santas) run

runLength :: SantaState -> Int
runLength (SantaState set _ _) = Set.size set

santaMove :: SantaState -> Char -> SantaState
santaMove (SantaState set seq ct) chr = let idx = ct `mod` (Seq.length seq)
                                            pos = Seq.index seq idx
                                            pos' = move pos chr
                                            seq' = Seq.update idx pos' seq in
                                     SantaState (Set.insert pos' set) seq' (idx + 1)

move :: Point -> Char -> Point
move (Pt x y) '>' = Pt (x+1) y
move (Pt x y) '<' = Pt (x-1) y
move (Pt x y) '^' = Pt x (y+1)
move (Pt x y) 'v' = Pt x (y-1)
move point _ = point

main = interact $ \input ->
    (printf "With 1 santa: %d\n"  $ santaRun 1 input) ++
    (printf "With 2 santas: %d\n" $ santaRun 2 input)
