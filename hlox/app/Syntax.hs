{-# LANGUAGE LambdaCase #-}

module Syntax where

data Expr
    = Grouping Expr
    | -- Literals
      LiteralNil
    | LiteralBoolean Bool
    | LiteralNumber Double
    | LiteralString String
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
    deriving (Eq, Show)

data Result = Nil | Boolean Bool | Number Double | String String
    deriving (Eq, Show)

evaluate :: Expr -> Maybe Result -- evaluate LiteralT
evaluate = \case
    Grouping expr -> evaluate expr
    -- Literals
    LiteralNil -> pure Nil
    LiteralBoolean b -> pure $ Boolean b
    LiteralNumber n -> pure $ Number n
    LiteralString s -> pure $ String s
    -- Boolean operators
    Not expr -> do
        result <- evaluate expr
        pure $ Boolean (isTruthy result)
    And left right -> do
        leftR <- evaluate left
        rightR <- evaluate right
        pure $ Boolean (isTruthy leftR && isTruthy rightR)
    Or left right -> do
        leftR <- evaluate left
        rightR <- evaluate right
        pure $ Boolean (isTruthy leftR || isTruthy rightR)
    -- Number operators
    Negate expr -> do
        result <- evaluate expr
        case result of
            Number n -> pure $ Number (-n)
            _ -> fail "Operand must be a number."
    Plus left right -> do
        leftR <- evaluate left
        rightR <- evaluate right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Number (l + r)
            (String l, String r) -> pure $ String (l ++ r)
            _ -> fail "Operands must be two numbers or two strings."
    Minus left right -> numberOperation (+) (evaluate left) (evaluate right)
    Star left right -> numberOperation (*) (evaluate left) (evaluate right)
    Slash left right -> numberOperation (/) (evaluate left) (evaluate right)
    -- Comparison operators
    Equal left right -> do
        leftR <- evaluate left
        rightR <- evaluate right
        pure $ Boolean (leftR == rightR)
    NotEqual left right -> numberComparison (/=) (evaluate left) (evaluate right)
    Less left right -> numberComparison (<) (evaluate left) (evaluate right)
    LessOrEqual left right -> numberComparison (<=) (evaluate left) (evaluate right)
    Greater left right -> numberComparison (>) (evaluate left) (evaluate right)
    GreaterOrEqual left right -> numberComparison (>=) (evaluate left) (evaluate right)
  where
    isTruthy expr = case expr of
        Nil -> False
        Boolean False -> False
        _ -> True
    numberOperation op left right = do
        leftR <- left
        rightR <- right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Number (l `op` r)
            _ -> fail "Operands must be numbers."
    numberComparison op left right = do
        leftR <- left
        rightR <- right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Boolean (l `op` r)
            _ -> fail "Operands must be numbers."

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
