{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hlox.Evaluate.Evaluators where

import Hlox.Evaluate.Eval (Eval, assignVariable, defineVariable, getVariable, printValue, withLocalScope)
import Hlox.Evaluate.Value (Value (..))
import Hlox.Syntax (
    Decl (..),
    Expr (..),
    Program (..),
    Stmt (..),
    VarDeclParams (..),
 )

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)

programEval :: Program -> Eval ()
programEval (Program declarations) = traverse_ declEval declarations

declEval :: Decl -> Eval ()
declEval = \case
    VarDecl params -> varDeclParamsEval params
    StmtDecl stmt -> stmtEval stmt

varDeclParamsEval :: VarDeclParams -> Eval ()
varDeclParamsEval (VarDeclParams name mExpr) = do
    value <- case mExpr of
        Just e -> exprEval e
        Nothing -> pure Nil
    defineVariable name value

stmtEval :: Stmt -> Eval ()
stmtEval = \case
    BlockStmt decls -> withLocalScope $ traverse_ declEval decls
    IfStmt cond thenStmt mElseStmt -> do
        test <- exprEval cond
        if isTruthy test
            then stmtEval thenStmt
            else traverse_ stmtEval mElseStmt
    WhileStmt cond body -> do
        test <- exprEval cond
        when (isTruthy test) $
            stmtEval body >> stmtEval (WhileStmt cond body)
    ForStmt mInit mCond mStep body -> do
        traverse_ (either varDeclParamsEval (void . exprEval)) mInit
        test <- maybe (pure $ Boolean True) exprEval mCond
        when (isTruthy test) $ do
            stmtEval body
            traverse_ exprEval mStep
            stmtEval (ForStmt Nothing mCond mStep body)
    PrintStmt expr -> exprEval expr >>= printValue
    ExprStmt expr -> void $ exprEval expr

exprEval :: Expr -> Eval Value
exprEval = \case
    Identifier name -> getVariable name
    -- Literals
    LiteralNil -> pure Nil
    LiteralBoolean b -> pure $ Boolean b
    LiteralNumber n -> pure $ Number n
    LiteralString s -> pure $ String s
    -- Boolean operators
    Not expr -> do
        result <- exprEval expr
        pure $ Boolean (isTruthy result)
    And left right -> do
        leftR <- exprEval left
        if not (isTruthy leftR)
            then pure leftR
            else exprEval right
    Or left right -> do
        leftR <- exprEval left
        if isTruthy leftR
            then pure leftR
            else exprEval right
    -- Number operators
    Negate expr -> do
        result <- exprEval expr
        case result of
            Number n -> pure $ Number (-n)
            _ -> throwError "Operand must be a number."
    Plus left right -> do
        leftR <- exprEval left
        rightR <- exprEval right
        case (leftR, rightR) of
            (Number l, Number r) -> pure $ Number (l + r)
            (String l, String r) -> pure $ String (l <> r)
            _ -> throwError "Operands must be two numbers or two strings."
    Minus left right -> numberOperation (-) (exprEval left) (exprEval right)
    Star left right -> numberOperation (*) (exprEval left) (exprEval right)
    Slash left right -> numberOperation (/) (exprEval left) (exprEval right)
    -- Comparison operators
    Equal left right -> do
        leftR <- exprEval left
        rightR <- exprEval right
        pure $ Boolean (leftR == rightR)
    NotEqual left right -> numberComparison (/=) (exprEval left) (exprEval right)
    Less left right -> numberComparison (<) (exprEval left) (exprEval right)
    LessOrEqual left right -> numberComparison (<=) (exprEval left) (exprEval right)
    Greater left right -> numberComparison (>) (exprEval left) (exprEval right)
    GreaterOrEqual left right -> numberComparison (>=) (exprEval left) (exprEval right)
    -- Assignment
    Assignment name expr -> do
        value <- exprEval expr
        assignVariable name value
    -- Parentheses
    Grouping expr -> exprEval expr
  where
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

-- Utilities

isTruthy :: Value -> Bool
isTruthy expr = case expr of
    Nil -> False
    Boolean False -> False
    _ -> True
