module Day7 ( parse, solve
             , Address(..)
             , Operation(..)
             , Instruction(..)
             , Assignment(..)
             , Wire(..)
             ) where

import Control.Monad.State.Lazy
import Data.Bits
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Word

data WordState = WS { vowels           :: Int,
                      doubleLetter     :: Bool,
                      forbiddenStrings :: Bool }

type Value = Word16
newtype Wire = Wire String deriving (Eq, Ord, Show)
data Address = Source Wire | Constant Value deriving (Eq)
data Operation = And | Or | LShift | RShift deriving (Eq, Show)
data Instruction = Val Address | Not Address | Op Operation Address Address deriving (Eq, Show)
data Assignment = Assign Instruction Wire deriving (Eq, Show)

type Circuit = Map.Map Wire Instruction

instance Show Address where
    show (Source (Wire wire)) = wire
    show (Constant value) = show value

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

solve :: [Assignment] -> Either String Circuit
solve assignments = let circuit = constructCircuit assignments in
    solve' 1000 circuit

solve' :: Int -> Circuit -> Either String Circuit
solve' 0 circuit = Left $ "Couldn't solve in 1000 iterations. Circuit state: " ++ show circuit
solve' c circuit = let (solved, circuit') = runState (fillIn $ Map.keys circuit) circuit in
    if solved
    then Right circuit'
    else solve' (c - 1) circuit'


fillIn :: [Wire] -> State Circuit Bool
fillIn (w:ws) = do
    circuit <- get
    put $ backfill w circuit
    fillIn ws

fillIn [] = do
    circuit <- get
    return $ Map.foldr ((&&) . isValConst) True circuit

isValConst :: Instruction -> Bool
isValConst (Val (Constant _)) = True
isValConst otherwise = False

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

getResults :: [Assignment] -> Either String Value
getResults assignments =  do
    circuit <- solve assignments
    case Map.lookup (Wire "a") circuit of
        Just (Val (Constant v)) -> Right v
        otherwise -> Left "Can't retieve constant for Wire a"

subB :: Value -> Assignment -> Assignment
subB v (Assign instr (Wire "b")) = Assign (Val (Constant v)) (Wire "b")
subB _ assign = assign


main = do
    file <- getContents
    let parsed = map parse $ lines file
    putStrLn $ intercalate "\n" $ lefts parsed
    let assignments = rights parsed
    case getResults assignments of
        Left err -> putStrLn err
        Right v -> do
            putStrLn $ "part a: " ++ (show v)
            let assignments' = map (subB v) assignments
            putStrLn $ either id (("part b: "++) . show) $ getResults assignments'
-- End execution
