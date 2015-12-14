module Day10 ( lookAndSay, repeatedLookAndSay ) where

lookAndSay :: String -> String
lookAndSay = lookAndSay' ""

lookAndSay' [] (next:rest) = lookAndSay' [next] rest
lookAndSay' buf@(last:_) (next:rest) | last == next = lookAndSay' (next:buf) rest
lookAndSay' buf@(last:_) (next:rest) | last /= next = (getSay buf) ++ lookAndSay' [next] rest
lookAndSay' buf [] = getSay buf

repeatApply :: Int -> (a -> a) -> a -> a
repeatApply 0 _ init = init
repeatApply x f init = f $ repeatApply (x - 1) f init

repeatedLookAndSay :: Int -> String -> String
repeatedLookAndSay count init = repeatApply count lookAndSay init

getSay :: String -> String
getSay run = (show $ length run) ++ [(head run)]

main = do
        let fourtyIters = repeatedLookAndSay 40 "1113222113"
        putStrLn $ "part a: " ++ show (length fourtyIters)
        let fiftyIters = repeatedLookAndSay 10 fourtyIters
        putStrLn $ "part b: " ++ show (length fiftyIters)



