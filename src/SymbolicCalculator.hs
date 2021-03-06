module SymbolicCalculator where

import           Data.Char

data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokNum Double
           | TokEnd
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
                  | c == '='        = TokAssign : tokenize cs
                  | c == '('        = TokLParen : tokenize cs
                  | c == ')'        = TokRParen : tokenize cs
                  | isDigit c       = number c cs
                  | isAlpha c       = identifier c cs
                  | isSpace c       = tokenize cs
                  | otherwise       = error $ "Cannot tokenize " ++ [c]

number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = span isDigit cs in TokNum (read (c : digs)) : tokenize cs'

identifier :: Char -> String -> [Token]
identifier c cs =
   let (str, cs') = span isAlphaNum cs in TokIdent (c : str) : tokenize cs'

---- parser ----

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
  deriving Show

lookAhead :: [Token] -> Token
lookAhead []       = TokEnd
lookAhead (c : cs) = c

accept :: [Token] -> [Token]
accept []       = error "Nothing to accept"
accept (t : ts) = ts

expression :: [Token] -> (Tree, [Token])
expression toks =
   let (termTree, toks') = term toks
   in  case lookAhead toks' of
              -- Term [+-] Expression
          (TokOp op) | elem op [Plus, Minus] ->
             let (exTree, toks'') = expression (accept toks')
             in  (SumNode op termTree exTree, toks'')
          -- Identifier '='  Expression
          TokAssign -> case termTree of
             VarNode str ->
                let (exTree, toks'') = expression (accept toks')
                in  (AssignNode str exTree, toks'')
             _ -> error "Only variables can be assigned to"
          -- Term
          _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
   let (facTree, toks') = factor toks
   in  case lookAhead toks' of
          (TokOp op) | elem op [Times, Div] ->
             let (termTree, toks'') = term (accept toks')
             in  (ProdNode op facTree termTree, toks'')
          _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks = case lookAhead toks of
   (TokNum   x  ) -> (NumNode x, accept toks)
   (TokIdent str) -> (VarNode str, accept toks)
   (TokOp op) | elem op [Plus, Minus] ->
      let (facTree, toks') = factor (accept toks)
      in  (UnaryNode op facTree, toks')
   TokLParen ->
      let (expTree, toks') = expression (accept toks)
      in  if lookAhead toks' /= TokRParen
             then error "Missing right parenthesis"
             else (expTree, accept toks')
   _ -> error $ "Parse error on token: " ++ show toks

parse :: [Token] -> Tree
parse toks =
   let (tree, toks') = expression toks
   in  if null toks' then tree else error $ "Leftover tokens: " ++ show toks'

evaluate :: Expression -> Double
evaluate = undefined

token :: Token
token = TokIdent "x"
