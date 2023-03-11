module Parser where

import Parser.Program (programP)
import Syntax (Program)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Megaparsec.Error (ParseErrorBundle)

type ParserError = ParseErrorBundle Text Void

parserErrorPretty :: ParserError -> Text
parserErrorPretty = T.pack . errorBundlePretty

parseProgram :: String -> Text -> Either ParserError Program
parseProgram = parse programP
