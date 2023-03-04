{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Eval (evaluateProgram)
import Eval.Common (evalEval)
import Eval.Environment (empty)
import Parser (programP)

import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.IO (stderr)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runFile path
        _ -> TIO.hPutStrLn stderr "exactly one argument expected"

runFile :: String -> IO ()
runFile path = do
    source <- TIO.readFile path
    case parse programP path source of
        Left err -> putStrLn $ errorBundlePretty err
        Right program -> do
            evalEval (evaluateProgram program) empty >>= \case
                Left err -> TIO.putStrLn err
                _ -> pure ()
