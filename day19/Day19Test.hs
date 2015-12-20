import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.HUnit
import Day19

calibration = Map.fromList [("H", ["HO", "OH"]), ("O", ["HH"])]
calibration' = Map.insert "e" ["H", "O"] calibration

test1 = TestCase (assertEqual "parsing example" expected $ parseCalibration string) where
    expected = Right ("HOH", calibration)
    string = unlines [ "H => HO"
                     , "H => OH"
                     , "O => HH"
                     , ""
                     , "HOH"
                     , "" -- trailing newline
                     ]

test2 = TestCase (assertEqual "combinations example" 4 $ substitutionCount "HOH" calibration')

test3 = TestCase (assertEqual string (Just 6) $ searchDepth string calibration') where
    string = "HOHOHO"

test4 = TestCase (assertEqual string (Just 3) $ searchDepth string calibration') where
    string = "HOH"

main = runTestTT $ TestList [ test1, test2, test3, test4 ]
