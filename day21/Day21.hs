module Day21 where

import Data.List (subsequences, sortBy, maximumBy, find)
import Data.Function (on)

data Item = Item { itemName   :: String
                 , itemCost   :: Int
                 , itemDamage :: Int
                 , itemArmour :: Int
                 } deriving (Eq, Show)

data Combatant = Combatant { cHitPoints :: Int
                           , cArmour    :: Int
                           , cDamage    :: Int
                           } deriving (Eq, Show)

type Loadout = [Item]

weaponList = [ Item "Dagger"        8     4       0
             , Item "Shortsword"   10     5       0
             , Item "Warhammer"    25     6       0
             , Item "Longsword"    40     7       0
             , Item "Greataxe"     74     8       0
             ]

armourList =  [ Item "Leather"     13     0       1
              , Item "Chainmail"   31     0       2
              , Item "Splintmail"  53     0       3
              , Item "Bandedmail"  75     0       4
              , Item "Platemail"  102     0       5
              ]

ringList = [ Item "Damage +1"    25     1       0
           , Item "Damage +2"    50     2       0
           , Item "Damage +3"   100     3       0
           , Item "Defense +1"   20     0       1
           , Item "Defense +2"   40     0       2
           , Item "Defense +3"   80     0       3
           ]

takeAtMost1 :: [a] -> [[a]]
takeAtMost1 []  = [[]]
takeAtMost1 (a:as) = [a]:(takeAtMost1 as)

takeAtMost2 :: [a] -> [[a]]
takeAtMost2 = (filter ((<= 2) . length)) . subsequences

gear :: [Loadout]
gear = [weapon:(armour ++ rings) | weapon <- weaponList
                                 , armour <- takeAtMost1 armourList
                                 , rings  <- takeAtMost2 ringList]

cost :: Loadout -> Int
cost = foldr ((+) . itemCost) 0

equipItem :: Combatant -> Item -> Combatant
equipItem c@Combatant {cArmour = armour, cDamage = damage} item =
        c { cArmour = itemArmour item + armour
          , cDamage = itemDamage item + damage }

equipLoadout :: Combatant -> Loadout -> Combatant
equipLoadout = foldl equipItem

data Outcome = Won | Lost deriving Eq

battle :: Combatant -> Combatant -> Outcome
battle me them =
        let them' = attack me them in
            if (cHitPoints them') <= 0
                then Won
                else let me' = attack them' me in
                    if (cHitPoints me') <= 0
                        then Lost
                        else battle me' them'
battleWithLoadout ::  Combatant -> Combatant -> Loadout -> Outcome
battleWithLoadout me them loadout = battle (equipLoadout me loadout) them

attack :: Combatant -> Combatant -> Combatant
attack attacker defender =
        let damage = max 1 $ (cDamage attacker) - (cArmour defender) in
            defender { cHitPoints = (cHitPoints defender) - damage }

findLowest :: Combatant -> Combatant -> Maybe Loadout
findLowest = let loadouts = sortBy (compare `on` cost) gear in
    findLowest' loadouts


findLowest' :: [Loadout] -> Combatant -> Combatant -> Maybe Loadout
findLowest' loadout me them = find ((==Won) . (battleWithLoadout me them)) loadout

findHighest me them = let f = battleWithLoadout me them
                          outcomes = zip (map f gear) (map cost gear)
                          outcomes' = filter ((==Lost) . fst) outcomes in
                              snd $ maximumBy (compare `on` snd) outcomes'
main = do
        let me = Combatant 100 0 0
        -- Parsers are for scrubs
        let boss = Combatant 103 2 9
        let (Just loadout) = findLowest me boss
        putStrLn $ "part a: " ++ (show $ cost loadout)
        let loadout' = findHighest me boss
        putStrLn $ "part b: " ++ (show $ loadout')
