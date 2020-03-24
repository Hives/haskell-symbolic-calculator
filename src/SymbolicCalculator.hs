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
                  | isDigit c       = number c cs
                  | isAlpha c       = identifier c cs
                  | isSpace c       = tokenize cs
                  | otherwise       = error $ "Cannot tokenize " ++ [c]

number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = digits cs in TokNum (read (c : digs)) : tokenize cs'

digits :: String -> (String, String)
digits str = digs "" str
 where
  digs acc [] = (acc, [])
  digs acc (c : cs)
     | isDigit c = let (acc', cs') = digs acc cs in (c : acc', cs')
     | otherwise = (acc, c : cs)

identifier :: Char -> String -> [Token]
identifier c cs =
   let (str, cs') = alnums cs in TokIdent (c : str) : tokenize cs'

alnums :: String -> (String, String)
alnums str = als "" str
 where
  als acc [] = (acc, [])
  als acc (c : cs)
     | isAlphaNum c = let (acc', cs') = als acc cs in (c : acc', cs')
     | otherwise    = (acc, c : cs)

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

token :: Token
token = TokIdent "x"
