module Day12 ( sumJSON ) where

import Text.JSON

sumJSON :: String -> Maybe String -> Int
sumJSON json ignore = case decodeStrict json of
                   Ok res -> sumJSON' ignore res
                   Error _ -> 0

sumJSON' :: Maybe String -> JSValue -> Int
sumJSON' _ (JSRational _ rat) = floor rat
sumJSON' ignore (JSArray arr) = foldr ((+) . (sumJSON' ignore)) 0 arr
sumJSON' ignore (JSObject obj) =
        let values = map snd $ fromJSObject obj :: [JSValue]
            skip = case ignore of
                       Nothing -> False
                       Just str -> any (JSString (toJSString str)==) values in
       if skip
           then 0
           else foldr ((+) . (sumJSON' ignore)) 0 values

sumJSON' _ (JSString _) = 0
sumJSON' _ (JSNull) = 0
sumJSON' _ (JSBool _) = 0

main = do
          json <- getContents
          let sum = sumJSON json Nothing
          putStrLn $ "part a: " ++ show sum
          let sum' = sumJSON json $ Just "red"
          putStrLn $ "part b: " ++ show sum'

