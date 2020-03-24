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

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan predicate = spanAcc []
 where
  spanAcc acc [] = (acc, [])
  spanAcc acc (c : cs)
     | predicate c = let (acc', cs') = spanAcc acc cs in (c : acc', cs')
     | otherwise   = (acc, c : cs)

digits :: String -> (String, String)
digits str = mySpan isDigit str

identifier :: Char -> String -> [Token]
identifier c cs =
   let (str, cs') = alnums cs in TokIdent (c : str) : tokenize cs'

alnums :: String -> (String, String)
alnums str = mySpan isAlphaNum str

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

token :: Token
token = TokIdent "x"
