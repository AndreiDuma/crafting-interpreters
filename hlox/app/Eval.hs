{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval (evaluateProgram) where

import Eval.Common (Eval, assignVariable, defineVariable, getVariable, printValue)
import Eval.Value (Value (..))
import Syntax (Declaration (..), Expr (..), Program (..), Stmt (..))

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)

-- TODO: create a Program -> Either RuntimeError Program function, then don't export the monad

evaluateProgram :: Program -> Eval ()
evaluateProgram (Program declarations) = traverse_ evaluateDeclaration declarations

evaluateDeclaration :: Declaration -> Eval ()
evaluateDeclaration = \case
    VarDecl name expr -> do
        value <- case expr of
            Just e -> evaluateExpr e
            Nothing -> pure Nil
        defineVariable name value
    Statement stmt -> evaluateStmt stmt

evaluateStmt :: Stmt -> Eval ()
evaluateStmt = \case
    ExprStmt expr -> void $ evaluateExpr expr
    PrintStmt expr -> evaluateExpr expr >>= printValue

evaluateExpr :: Expr -> Eval Value
evaluateExpr = \case
    Identifier name -> getVariable name
    -- Literals
    LiteralNil -> pure Nil
    LiteralBoolean b -> pure $ Boolean b
    LiteralNumber n -> pure $ Number n
    LiteralString s -> pure $ String s
    -- Boolean operators
    Not expr -> do
        result <- evaluateExpr expr
        pure $ Boolean (isTruthy result)
    And left right -> do
        leftR <- evaluateExpr left
        if not (isTruthy leftR)
            then pure leftR
            else evaluateExpr right
    Or left right -> do
        leftR <- evaluateExpr left
        if isTruthy leftR
            then pure leftR
            else evaluateExpr right
    -- Number operators
    Negate expr -> do
        result <- evaluateExpr expr
        case result of
            Number n -> pure $ Number (-n)
            _ -> throwError "Operand must be a number."
    Plus left right -> do
        leftR <- evaluateExpr left
        rightR <- evaluateExpr right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Number (l + r)
            (String l, String r) -> pure $ String (l <> r)
            _ -> throwError "Operands must be two numbers or two strings."
    Minus left right -> numberOperation (+) (evaluateExpr left) (evaluateExpr right)
    Star left right -> numberOperation (*) (evaluateExpr left) (evaluateExpr right)
    Slash left right -> numberOperation (/) (evaluateExpr left) (evaluateExpr right)
    -- Comparison operators
    Equal left right -> do
        leftR <- evaluateExpr left
        rightR <- evaluateExpr right
        pure $ Boolean (leftR == rightR)
    NotEqual left right -> numberComparison (/=) (evaluateExpr left) (evaluateExpr right)
    Less left right -> numberComparison (<) (evaluateExpr left) (evaluateExpr right)
    LessOrEqual left right -> numberComparison (<=) (evaluateExpr left) (evaluateExpr right)
    Greater left right -> numberComparison (>) (evaluateExpr left) (evaluateExpr right)
    GreaterOrEqual left right -> numberComparison (>=) (evaluateExpr left) (evaluateExpr right)
    -- Assignment
    Assignment name expr -> do
        value <- evaluateExpr expr
        assignVariable name value
    -- Parentheses
    Grouping expr -> evaluateExpr expr
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
