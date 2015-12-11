module Day5 (isNice) where

import Data.Word
import qualified Data.Map.Strict as Map
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data WordState = WS { vowels           :: Int,
                      doubleLetter     :: Bool,
                      forbiddenStrings :: Bool }

type Value = Word16
data Address = Register String | Constant Value deriving Show
data Operation = And | Or | LShift | RShift deriving Show
data Instruction = Val Address | Not Address | Duo Operation Address Address deriving Show
data Assignment = Assign Instruction Address

def = emptyDef{ identStart = lower
              , identLetter = lower
              , opStart = upper <|> char '-'
              , opLetter = upper <|> char '>'
              , reservedOpNames = ["NOT", "LSHIFT", "RSHIFT", "AND", "OR", "->"]
              , caseSensitive = True
              }
TokenParser { identifier = m_identifier
            , reservedOp = m_reservedOp
            , whiteSpace = m_whiteSpace
            , natural = m_natural
            } = makeTokenParser def

exprParser :: Parser Instruction
exprParser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "NOT"    >> return Not)]
        , [Infix  (m_reservedOp "AND"    >> return (Duo And)) AssocNone]
        , [Infix  (m_reservedOp "OR"     >> return (Duo Or)) AssocNone]
        , [Infix  (m_reservedOp "RSHIFT" >> return (Duo RShift)) AssocNone]
        , [Infix  (m_reservedOp "LSHIFT" >> return (Duo LShift)) AssocNone]
        ]

term = fmap Val (Register m_identifier)
       <|> fmap Val (Constant m_natural)

mainparser :: Parser Assignment
mainparser = m_whiteSpace >> stmt1 <* eof
    where
      stmt1 = do { r <- m_identifier
                     ; m_reservedOp "->"
                     ; i <- exprParser
                     ; return (Assign i r)
                     }
