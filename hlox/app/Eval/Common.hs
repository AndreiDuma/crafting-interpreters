{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval.Common where

import Eval.Environment (Environment, assignVar, defineVar, getVar)
import Eval.Result (Result (..))

import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify', put, runStateT)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Eval a = ExceptT Text (StateT Environment IO) a

runEval :: Eval a -> Environment -> IO (Either Text a, Environment)
runEval e = runStateT (runExceptT e)

evalEval :: Eval a -> Environment -> IO (Either Text a)
evalEval e = evalStateT (runExceptT e)

getVariable :: Text -> Eval Result
getVariable name = do
    env <- get
    case getVar name env of
        Just result -> pure result
        Nothing -> throwError ("Undefined variable " <> name <> ".")

defineVariable :: Text -> Result -> Eval ()
defineVariable name value = modify' (defineVar name value)

assignVariable :: Text -> Result -> Eval Result
assignVariable name value = do
    env <- get
    case assignVar name value env of
        Just env' -> put env' >> pure value
        Nothing -> throwError ("Undefined variable " <> name <> ".")

printValue :: Result -> Eval ()
printValue =
    liftIO . TIO.putStrLn . \case
        Nil -> "nil"
        Boolean True -> "true"
        Boolean False -> "false"
        Number d -> T.pack (show d)
        String t -> t
