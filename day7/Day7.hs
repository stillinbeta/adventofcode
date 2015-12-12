module Day7 (parse, Address(..), Operation(..), Instruction(..), Assignment(..)) where

import Data.List.Split
import Data.Word
import Data.Char
import qualified Data.Map.Strict as Map

data WordState = WS { vowels           :: Int,
                      doubleLetter     :: Bool,
                      forbiddenStrings :: Bool }

type Value = Word16
data Address = Register String | Constant Value deriving (Eq, Show)
data Operation = And | Or | LShift | RShift deriving (Eq, Show)
data Instruction = Val Address | Not Address | Op Operation Address Address deriving (Eq, Show)
data Assignment = Assign Instruction Address deriving (Eq, Show)

isConstant :: String -> Bool
isConstant = foldr ((&&) . isNumber) True

isRegister :: String -> Bool
isRegister = foldr ((&&) . isLower) True

toOp :: String -> Either String Operation
toOp "AND" = Right And
toOp "OR" = Right Or
toOp "LSHIFT" = Right LShift
toOp "RSHIFT" = Right RShift
toOp op = Left $ "Can't parse op \"" ++ op ++ "\""

-- not strictly correct (should be all alpha or all num), but eh.
isAddress :: String -> Bool
isAddress = foldr ((&&) . isAlphaNum) True

toAddress :: String -> Either String Address
toAddress a | isConstant a = Right $ Constant (read a)
toAddress a | isRegister a = Right $ Register a
toAddress a = Left $ "can't parse \"" ++ a ++ "\"to address"

parse :: String -> Either String Assignment
parse = parse' . (splitOn " ")

parse' :: [String] -> Either String Assignment
parse' [a1, op, a2, "->", a3] = do
    a1' <- toAddress a1
    a2' <- toAddress a2
    a3' <- toAddress a3
    op' <- toOp op
    return $ Assign (Op op' a1' a2') a3'
parse' ["NOT", a1, "->", a2] = do
    a1' <- toAddress a1
    a2' <- toAddress a2
    return $ Assign (Not a1') a2'
parse' [a1, "->", a2] = do
    a1' <- toAddress a1
    a2' <- toAddress a2
    return $ Assign (Val a1') a2'
parse' xs = Left $ "can't parse " ++ show xs
