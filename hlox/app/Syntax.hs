module Syntax where

import Data.Text (Text)

newtype Program = Program [Declaration]

data Declaration
    = VarDeclaration Text Expr -- TODO
    | Statement Stmt -- TODO
    deriving (Eq, Show)

data Stmt
    = ExprStmt Expr -- TODO
    | PrintStmt Expr -- TODO
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

-- data Expr
--     = LiteralExpr LiteralExpr
--     | UnaryExpr UnaryExpr
--     | BinaryExpr BinaryExpr
--     | GroupingExpr Expr
--     deriving (Eq, Show)

-- data LiteralExpr
--     = LiteralTrue
--     | LiteralFalse
--     | LiteralNil
--     | LiteralNumber Double
--     | LiteralString String
--     deriving (Eq, Show)

-- data UnaryExpr
--     = UnaryMinus Expr
--     | Bang Expr
--     deriving (Eq, Show)

-- data BinaryExpr
--     = Expr `Plus` Expr
--     | Expr `Minus` Expr
--     | Expr `Star` Expr
--     | Expr `Slash` Expr
--     | Expr `And` Expr
--     | Expr `Or` Expr
--     | Expr `Equal` Expr
--     | Expr `NotEqual` Expr
--     | Expr `LessThan` Expr
--     | Expr `LessThanOrEqual` Expr
--     | Expr `GreaterThan` Expr
--     | Expr `GreaterThanOrEqual` Expr
--     deriving (Eq, Show)
