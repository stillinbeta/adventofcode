{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Day23 (parseProgram, runProgram, Instruction(..), Register(..)) where

import qualified Control.Monad.State.Lazy as State
import Data.Label (mkLabel, get, set)
import Data.Label.Monadic (gets, modify)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)

data Register = A | B deriving (Eq, Show)

type Offset = Int

data Instruction = Hlf Register
                 | Tpl Register
                 | Inc Register
                 | Jmp Offset
                 | Jie Register Offset
                 | Jio Register Offset deriving (Eq, Show)

p_program =
        let instrs = [p_hlf, p_tpl, p_inc, p_jmp, p_jie, p_jio] in
        endBy (choice $ map try instrs) newline

p_hlf = do
        string "hlf "
        register <- p_register
        return $ Hlf register

p_tpl = do
        string "tpl "
        register <- p_register
        return $ Tpl register

p_inc = do
        string "inc "
        register <- p_register
        return $ Inc register

p_jmp = do
        string "jmp "
        offset <- p_offset
        return $ Jmp offset

p_jie = do
        string "jie "
        register <- p_register
        string ", "
        offset <- p_offset
        return $ Jie register offset

p_jio = do
        string "jio "
        register <- p_register
        string ", "
        offset <- p_offset
        return $ Jio register offset

p_offset = int

p_register = do
        letter <- oneOf "ab"
        case letter of
            'a' -> return A
            'b' -> return B

parseProgram = parse p_program "?"

data GameState = GameState { _regA         :: Word
                           , _regB         :: Word
                           , _nextPos      :: Int
                           , _instructions :: [Instruction]
                           }
mkLabel ''GameState

instance Show GameState where
        show gs = "reg a: " ++ (show $ get regA gs) ++ " reg b: " ++ (show $ get regB gs) ++ " next: " ++ (show $ get nextPos gs)


newState :: [Instruction] -> GameState
newState instrs = GameState { _regA = 0
                            , _regB = 0
                            , _nextPos = 0
                            , _instructions = instrs }

getRegister :: Register -> State.State GameState Word
getRegister reg =
        gets $ case reg of
                 A -> regA
                 B -> regB

modifyRegister :: Register -> (Word -> Word) -> State.State GameState ()
modifyRegister reg f = case reg of
                           A -> modify regA f
                           B -> modify regB f

getInstruction :: State.State GameState (Maybe Instruction)
getInstruction = do
        pos <- gets nextPos
        instrs <- gets instructions
        if pos < length instrs && pos >= 0
            then return $ Just (instrs !! pos)
            else return Nothing


runInstruction :: Instruction -> State.State GameState ()
runInstruction (Hlf r) = do
        modifyRegister r (`div` 2)
        modify nextPos (+1)
runInstruction (Tpl r) = do
        modifyRegister r (*3)
        modify nextPos (+1)
runInstruction (Inc r) = do
        modifyRegister r (+1)
        modify nextPos (+1)
runInstruction (Jmp offset) =
        modify nextPos (+offset)
runInstruction (Jie r offset) = do
        val <- getRegister r
        let diff = if val `rem` 2 == 0 then offset else 1
        modify nextPos (+diff)
runInstruction (Jio r offset) = do
        val <- getRegister r
        let diff = if val == 1 then offset else 1
        modify nextPos (+diff)

execute :: State.State GameState Word
execute = do
        instr <- getInstruction
        case instr of
            Nothing -> getRegister B
            Just instr' -> do
               runInstruction instr'
               state <- State.get
               execute

runProgram :: GameState -> Word
runProgram = State.evalState execute

main = do
        contents <- getContents
        let parsed = parseProgram contents
        case parsed of
            Left e -> putStrLn $ show e
            Right instrs -> do
                let state = newState instrs
                putStrLn $ "part a: " ++ (show $ runProgram state)
                let state' = set regA 1 state
                putStrLn $ "part b: " ++ (show $ runProgram state')
