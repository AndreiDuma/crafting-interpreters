module Parser (
    Parser,
    ParserError,
    parseProgram,
    parserErrorPretty,
) where

import Parser.Parsers (programP)
import Parser.Type (Parser, ParserError)
import Syntax (Program)

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (errorBundlePretty, parse)

parserErrorPretty :: ParserError -> Text
parserErrorPretty = T.pack . errorBundlePretty

parseProgram :: String -> Text -> Either ParserError Program
parseProgram = parse programP
