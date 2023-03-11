{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hlox.Evaluate.Eval (
    Eval,
    evalEval,
    assignVariable,
    defineVariable,
    getVariable,
    printValue,
) where

import Hlox.Evaluate.Environment (Environment)
import Hlox.Evaluate.Environment qualified as Env
import Hlox.Evaluate.Value (Value (..))

import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify', put)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Eval a = ExceptT Text (StateT Environment IO) a

evalEval :: Eval a -> Environment -> IO (Either Text a)
evalEval e = evalStateT (runExceptT e)

assignVariable :: Text -> Value -> Eval Value
assignVariable name value = do
    env <- get
    case Env.assignVariable name value env of
        Just env' -> put env' >> pure value
        Nothing -> throwError ("Undefined variable " <> name <> ".")

defineVariable :: Text -> Value -> Eval ()
defineVariable name value = modify' (Env.defineVariable name value)

getVariable :: Text -> Eval Value
getVariable name = do
    env <- get
    case Env.getVariable name env of
        Just result -> pure result
        Nothing -> throwError ("Undefined variable " <> name <> ".")

printValue :: Value -> Eval ()
printValue =
    liftIO . TIO.putStrLn . \case
        Nil -> "nil"
        Boolean True -> "true"
        Boolean False -> "false"
        Number d -> T.pack (show d)
        String t -> t
