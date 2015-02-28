module Language.PhiPlot.AST where

import Prelude hiding (Ord(..))

type Name = String

data AST = Assign Name Expr
         | Def  Name [Expr] [AST]
         | For Name Expr Expr Expr [AST]
         | If BoolExpr [AST] [AST]
         | While BoolExpr [AST]
         | Return Expr
         | AExp Expr
         | BExp BoolExpr
         | Void
         deriving (Eq, Show)

data Expr = Var Name
          | Number Double
          | UniOp UniOp Expr
          | BinOp Op Expr Expr
          | Call Name [Expr]
          | Fcall Name [Expr]
          deriving (Eq, Show)

data BoolExpr = BoolVal Bool
              | Cmp Op BoolExpr BoolExpr
              | LogicOp Op BoolExpr BoolExpr
              | Not BoolExpr
              | Nonzero Expr
              | BEAtrom Expr
              deriving (Eq, Show)

data Op = Plus | Minus
        | Mul  | Div
        | LT | GT | LE | GE | EQ | NE 
        | AND | OR
        deriving (Eq, Show)

data UniOp = Negative | Positive | NOT deriving (Eq, Show) 
