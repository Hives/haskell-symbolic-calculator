module SymbolicCalculator where

import           Data.Char

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           deriving (Show, Eq)

showContent :: Token -> String
showContent (TokOp    op ) = opToStr op
showContent (TokIdent str) = str
showContent (TokNum   i  ) = show i

data Expression

data Operator = Plus | Minus | Times | Div
   deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

opToStr :: Operator -> String
opToStr Plus  = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div   = "/"

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) | c `elem` "+-*/" = TokOp (operator c) : tokenize cs
                  | isDigit c       = TokNum (digitToInt c) : tokenize cs
                  | isAlpha c       = TokIdent [c] : tokenize cs
                  | isSpace c       = tokenize cs
                  | otherwise       = error $ "Cannot tokenize " ++ [c]

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

token :: Token
token = TokIdent "x"
