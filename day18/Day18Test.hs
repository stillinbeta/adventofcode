import Test.HUnit
import Day18
import qualified Data.Sequence as Seq

grid = let lg = Seq.fromList [ Seq.fromList [ Off, On,  Off, On,  Off, On  ]
                             , Seq.fromList [ Off, Off, Off, On,  On,  Off ]
                             , Seq.fromList [ On,  Off, Off, Off, Off, On  ]
                             , Seq.fromList [ Off, Off, On,  Off, Off, Off ]
                             , Seq.fromList [ On,  Off, On,  Off, Off, On  ]
                             , Seq.fromList [ On,  On,  On,  On,  Off, Off ]
                             ] in LightGrid 6 6 lg
grid' = let lg = Seq.fromList [ Seq.fromList [Off, Off, On,  On,  Off, Off]
                              , Seq.fromList [Off, Off, On,  On,  Off, On ]
                              , Seq.fromList [Off, Off, Off, On,  On,  Off]
                              , Seq.fromList [Off, Off, Off, Off, Off, Off]
                              , Seq.fromList [On,  Off, Off, Off, Off, Off]
                              , Seq.fromList [On,  Off, On,  On,  Off, Off]
                              ] in LightGrid 6 6 lg

test1 = TestCase (assertEqual "parsing example" (Right grid) $ parseGrid string) where
    string = unlines [ ".#.#.#"
                     , "...##."
                     , "#....#"
                     , "..#..."
                     , "#.#..#"
                     , "####.."
                     , "" -- trailing newline
                     ]

test2 = TestCase (assertEqual "transition example 1" grid' $ nextGrid grid)

main = runTestTT $ TestList [ test1, test2 ]
