module Day7 (parse, Address(..), Operation(..), Instruction(..), Assignment(..), Wire(..)) where

import Control.Monad.State.Lazy
import Data.Bits
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Word

data WordState = WS { vowels           :: Int,
                      doubleLetter     :: Bool,
                      forbiddenStrings :: Bool }

type Value = Word16
newtype Wire = Wire String deriving (Eq, Show, Ord)
data Address = Source Wire | Constant Value deriving (Eq, Show)
data Operation = And | Or | LShift | RShift deriving (Eq, Show)
data Instruction = Val Address | Not Address | Op Operation Address Address deriving (Eq, Show)
data Assignment = Assign Instruction Wire deriving (Eq, Show)

type Circuit = Map.Map Wire Instruction

-- Begin parsing

isConstant :: String -> Bool
isConstant = foldr ((&&) . isNumber) True

isWire :: String -> Bool
isWire = foldr ((&&) . isLower) True

isAddress :: String -> Bool
isAddress = foldr ((&&) . isAlphaNum) True

toOp :: String -> Either String Operation
toOp "AND" = Right And
toOp "OR" = Right Or
toOp "LSHIFT" = Right LShift
toOp "RSHIFT" = Right RShift
toOp op = Left $ "Can't parse op \"" ++ op ++ "\""

-- not strictly correct (should be all alpha or all num), but eh.
toWire :: String -> Either String Wire
toWire w | isWire w = Right $ Wire w
toWire w = Left $ "can't parse \"" ++ w ++"\" to wire"

toAddress :: String -> Either String Address
toAddress a | isConstant a = Right $ Constant (read a)
toAddress a | isWire a = Right $ Source (Wire a)
toAddress a = Left $ "can't parse \"" ++ a ++ "\"to address"

parse :: String -> Either String Assignment
parse = parse' . words

parse' :: [String] -> Either String Assignment
parse' [a1, op, a2, "->", a3] = do
    a1' <- toAddress a1
    a2' <- toAddress a2
    a3' <- toWire a3
    op' <- toOp op
    return $ Assign (Op op' a1' a2') a3'
parse' ["NOT", a1, "->", a2] = do
    a1' <- toAddress a1
    a2' <- toWire a2
    return $ Assign (Not a1') a2'
parse' [a1, "->", a2] = do
    a1' <- toAddress a1
    a2' <- toWire a2
    return $ Assign (Val a1') a2'
parse' xs = Left $ "can't parse " ++ show xs

-- End parsing
-- Begin Solving

--solve :: [Assignment] -> Circuit

--solve' :: [

--fillIn :: [Wire] -> State Circuit Boolean
--fillIn (w:ws) = --do
--    set <- getState

getConstant :: Address -> Circuit -> Maybe Value
getConstant (Source wire) circuit = do
    instr <- Map.lookup wire circuit
    (case instr of
        (Val (Constant val)) -> Just val
        otherwise -> Nothing)
getConstant (Constant value) _ = Just value

eval :: Instruction -> Circuit -> Maybe Value
eval (Val a) circuit = do
    val <- getConstant a circuit
    return val
eval (Not a) circuit = do
    val <- getConstant a circuit
    return $ complement val
eval (Op op a1 a2) circuit = do
    val1 <- getConstant a1 circuit
    val2 <- getConstant a2 circuit
    return $ case op of
        And -> val1 .&. val2
        Or ->  val1 .|. val2
        RShift -> val1 `shiftR` (fromIntegral val2)
        LShift -> val1 `shiftL` (fromIntegral val2)

backfill :: Wire -> Circuit -> Circuit
backfill wire circuit =
    let newInstr = do
        instr <- Map.lookup wire circuit
        val <- eval instr circuit
        return val in
    case newInstr of
        Just v -> Map.insert wire (Val (Constant v)) circuit
        Nothing -> circuit

constructCircuit :: [Assignment] -> Circuit
constructCircuit ((Assign instr wire):as) = Map.insert wire instr $ constructCircuit as
constructCircuit [] = Map.empty

-- End Solving
-- Begin execution
main = do
    file <- getContents
    let results = map parse $ splitOn "\n" file
    putStrLn $ intercalate "\n" $ lefts results

-- End execution
