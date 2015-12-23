{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--module Day22 where

import qualified Control.Monad.State.Lazy as State
import Data.Label (get, set, modify, mkLabel)
import Data.List (findIndex, findIndices, sort)
import Control.Monad (when)
import Data.Maybe (isJust, mapMaybe, listToMaybe)

-- Data Model --
data StatusEffect = Shield | Poison | Recharge deriving (Eq, Show)

data GameState = GState { _sPlayerHP     :: Int
                        , _sPlayerMana   :: Int
                        , _sTotalMana    :: Int
                        , _sDifficulty   :: Int
                        , _sBossHP       :: Int
                        , _sBossAttack   :: Int
                        , _sStatuses     :: [(StatusEffect, Int)]
                        } deriving Show
mkLabel ''GameState

type Battle = State.State GameState (Maybe Int)
type Action = State.State GameState ()

modify' l f = State.modify $ modify l f
get' = State.gets . get

-- End Data Model

-- Spell helpers --
damageBoss :: Int -> Action
damageBoss i = modify' sBossHP (subtract i)

manaCost :: Int -> Action
manaCost i = do
        modify' sPlayerMana (subtract i)
        modify' sTotalMana  (+i)

statusEffect :: StatusEffect -> Int -> Action
statusEffect effect duration = modify' sStatuses ((effect, duration):)
-- End spell helpers --
-- Spells --
magicMissile :: Action
magicMissile = do
        damageBoss 4
        manaCost 53

drain :: Action
drain  = do
        damageBoss 2
        manaCost 73
        modify' sPlayerHP (+2)

shield :: Action
shield = do
        manaCost 113
        statusEffect Shield 6

poison :: Action
poison = do
        manaCost 173
        statusEffect Poison 6

recharge :: Action
recharge = do
        manaCost 229
        statusEffect Recharge 5
-- End spells --

damagePlayer :: Action
damagePlayer = do
        statuses <- get' sStatuses
        let armour = if hasEffect Shield statuses
                         then 7
                         else 0
        bossDamage <- get' sBossAttack
        let bossDamage' = max 1 (bossDamage - armour)
        modify' sPlayerHP (subtract bossDamage')


-- Turn phases --
updateStatusEffects :: [(StatusEffect, Int)] -> [(StatusEffect, Int)]
updateStatusEffects ((effect, count):rest) =
        if count == 1
            then updateStatusEffects rest
            else (effect, count - 1):(updateStatusEffects rest)
updateStatusEffects [] = []

hasEffect e statuses = isJust $ findIndex ((==e) . fst) statuses

cast :: Action -> Battle
cast spell = do
        spell
        checkCast bossTurn
checkCast :: Battle -> Battle
checkCast next = do
        mana <- get' sPlayerMana
        statuses <- get' sStatuses
        let shieldCount = length $ findIndices ((==Shield) . fst) statuses
        let poisonCount = length $ findIndices ((==Poison) . fst) statuses
        let rechargeCount = length $ findIndices ((==Recharge) . fst) statuses
        if mana <= 0 || shieldCount >= 2 || poisonCount >= 2 || rechargeCount >= 2
            then return Nothing
            else next
applyEffects :: Action
applyEffects = do
        statuses <- get' sStatuses
        when (hasEffect Recharge statuses) (modify' sPlayerMana (+101))
        when (hasEffect Poison statuses) (modify' sBossHP (subtract 3))
        modify' sStatuses updateStatusEffects

applyDifficulty :: Action
applyDifficulty = do
                difficulty <- get' sDifficulty
                modify' sPlayerHP (subtract difficulty)
-- End Turn Phases
-- Turns --
bossTurn :: Battle
bossTurn = do
        applyEffects
        hp <- get' sBossHP
        if hp <= 0
            then do
                manaUsed <- get' sTotalMana
                return $ Just manaUsed
            else do
                damagePlayer
                playerTurn

playerTurn :: Battle
playerTurn = do
        applyDifficulty
        hp <- get' sPlayerHP
        if hp <= 0
            then return Nothing
            else do
                applyEffects
                state <- State.get
                let f = \spell -> State.evalState (cast spell) state
                let results = mapMaybe f [magicMissile, drain, shield, poison, recharge]
                let resultsSorted = sort results
                return $ listToMaybe resultsSorted
-- End turns --

state = GState { _sPlayerMana = 500
                , _sPlayerHP = 50
                , _sTotalMana = 0
                , _sStatuses = []
                , _sDifficulty = 0
                -- Input
                , _sBossHP = 71
                , _sBossAttack = 10
                }

main = do
        let lowestManaA = State.evalState playerTurn state
        putStrLn $ "part a: " ++ maybe "not found" show lowestManaA
        let state' = set sDifficulty 1 state
        let lowestManaB = State.evalState playerTurn state'
        putStrLn $ "part b: " ++ maybe "not found" show lowestManaB
