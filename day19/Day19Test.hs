import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.HUnit
import Day19

calibration = Map.fromList [("H", ["HO", "OH"]), ("O", ["HH"])]

test1 = TestCase (assertEqual "parsing example" expected $ parseCalibration string) where
    expected = Right ("HOH", calibration)
    string = unlines [ "H => HO"
                     , "H => OH"
                     , "O => HH"
                     , ""
                     , "HOH"
                     , "" -- trailing newline
                     ]

test2 = TestCase (assertEqual "combinations example" 4 $ substitutionCount "HOH" calibration)

main = runTestTT $ TestList [ test1, test2 ]
