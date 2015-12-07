module Day4 (findHash, checkGuess) where

import qualified Data.Hash.MD5 as MD5
import Data.List

findHash :: String -> Int -> Int
findHash = findHash' 1

findHash' :: Int -> String -> Int -> Int
findHash' num key zeroes = if checkGuess num key zeroes
                           then num
                           else findHash' (num + 1) key zeroes

getZeros :: Int -> String
getZeros count = replicate count '0'

checkGuess :: Int -> String -> Int -> Bool
checkGuess num key zeroes = let guess = MD5.Str $ key ++ (show num)
                                prefix = getZeros zeroes in
                     isPrefixOf prefix $ MD5.md5s guess

main = let key = "ckczppom" in
       do putStrLn $ "00000 -> " ++ ( show $ findHash key 5 )
          putStrLn $ "000000 -> " ++ ( show $ findHash key 6 )
