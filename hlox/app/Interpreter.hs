{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExcept)
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import Syntax (Expr (..))

data Result = Nil | Boolean Bool | Number Double | String Text
    deriving (Eq, Show)

type Eval a = ExceptT Text Identity a

runEval :: Eval a -> Either Text a
runEval = runExcept

evaluate :: Expr -> Eval Result
evaluate = \case
    Identifier name -> pure Nil -- TODO: read value from environment
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
        if not (isTruthy leftR)
            then pure leftR
            else evaluate right
    Or left right -> do
        leftR <- evaluate left
        if isTruthy leftR
            then pure leftR
            else evaluate right
    -- Number operators
    Negate expr -> do
        result <- evaluate expr
        case result of
            Number n -> pure $ Number (-n)
            _ -> throwError "Operand must be a number."
    Plus left right -> do
        leftR <- evaluate left
        rightR <- evaluate right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Number (l + r)
            (String l, String r) -> pure $ String (l <> r)
            _ -> throwError "Operands must be two numbers or two strings."
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
    -- Assignment
    Assignment name expr -> do
        result <- evaluate expr
        let _ = result -- TODO: write value to environment
        pure result
    -- Parentheses
    Grouping expr -> evaluate expr
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
            _ -> throwError "Operands must be numbers."
    numberComparison op left right = do
        leftR <- left
        rightR <- right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Boolean (l `op` r)
            _ -> throwError "Operands must be numbers."
