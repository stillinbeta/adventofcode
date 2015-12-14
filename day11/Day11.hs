module Day11 ( validPass, nextPass, nextValidPass ) where

import qualified Data.Set as Set
import Data.Char

import Debug.Trace

showTrace str x = trace (str ++ show x) x

validPass :: String -> Bool
validPass s = validPass1 s && validPass2 s && validPass3 s

validPass1 (c1:c2:c3:rest)
        | c2 == nextChar c1 && c3 == nextChar c2 && c1 <= 'x' = True
        | otherwise = validPass1 (c2:c3:rest)
validPass1 _ = False

validPass2 :: String -> Bool
validPass2 ('i':_) = False
validPass2 ('o':_) = False
validPass2 ('l':_) = False
validPass2 (_:rest) = validPass2 rest
validPass2 [] = True

validPass3 :: String -> Bool
validPass3 = validPass3' Set.empty

validPass3' :: Set.Set (Char, Char) -> String -> Bool
validPass3' set (c1:c2:rest)
        | c1 == c2 = validPass3' (Set.insert (c1, c2) set) (c2:rest)
        | otherwise = validPass3' set (c2:rest)
validPass3' set _ = Set.size set >= 2

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar c | isLower c = chr $ ord c + 1

nextPass s = reverse (nextPass' $ reverse s)
nextPass' (c:cs) =
                let c' = nextChar c in
                if c' == 'a'
                then c':nextPass' cs
                else c':cs

nextValidPass :: String -> String
nextValidPass pass = let pass' = nextPass pass in
        if validPass pass'
                then pass'
                else nextValidPass pass'

main = do
    let pass = nextValidPass "hepxcrrq"
    putStrLn $ "part a: " ++ pass
    let pass' = nextValidPass pass
    putStrLn $ "part b: " ++ pass'
