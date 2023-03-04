module Syntax where

import Data.Text (Text)

newtype Program = Program [Declaration]
    deriving (Eq, Show)

data Declaration
    = VarDecl Text (Maybe Expr)
    | Statement Stmt
    deriving (Eq, Show)

data Stmt
    = ExprStmt Expr
    | PrintStmt Expr
    deriving (Eq, Show)

data Expr
    = Identifier Text
    | -- Literals
      LiteralNil
    | LiteralBoolean Bool
    | LiteralNumber Double
    | LiteralString Text
    | -- Boolean operators
      Not Expr
    | And Expr Expr
    | Or Expr Expr
    | -- Number operators
      Negate Expr
    | Plus Expr Expr
    | Minus Expr Expr
    | Star Expr Expr
    | Slash Expr Expr
    | -- Comparison operators
      Equal Expr Expr
    | NotEqual Expr Expr
    | Less Expr Expr
    | LessOrEqual Expr Expr
    | Greater Expr Expr
    | GreaterOrEqual Expr Expr
    | -- Assignment
      Assignment Text Expr
    | -- Parentheses
      Grouping Expr
    deriving (Eq, Show)
