module Hlox.Syntax (
    Program (..),
    Decl (..),
    Stmt (..),
    Expr (..),
) where

import Data.Text (Text)

newtype Program = Program [Decl]
    deriving (Eq, Show)

data Decl
    = VarDecl Text (Maybe Expr)
    | StmtDecl Stmt
    deriving (Eq, Show)

data Stmt
    = BlockStmt [Decl]
    | PrintStmt Expr
    | ExprStmt Expr
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
