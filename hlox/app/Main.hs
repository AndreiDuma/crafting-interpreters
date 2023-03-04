{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as T
import Parser (pProgram)
import System.Environment (getArgs)
import System.IO (stderr)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runFile path
        _ -> T.hPutStrLn stderr "exactly one argument expected"

runFile :: String -> IO ()
runFile path = do
    source <- T.readFile path
    print source
    case parse pProgram path source of
        Left err -> putStrLn $ errorBundlePretty err
        Right result -> print result
