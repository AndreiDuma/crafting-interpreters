{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hlox.Evaluate (evaluateProgram)
import Hlox.Parse (parseProgram, parserErrorPretty)

import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runFile path
        _ -> TIO.hPutStrLn stderr "exactly one argument expected"

runFile :: String -> IO ()
runFile path = do
    source <- TIO.readFile path
    program <- case parseProgram path source of
        Left err -> TIO.hPutStrLn stderr (parserErrorPretty err) >> exitFailure
        Right program -> pure program
    -- print program
    evaluateProgram program >>= \case
        Left err -> TIO.hPutStrLn stderr err
        _ -> pure ()
