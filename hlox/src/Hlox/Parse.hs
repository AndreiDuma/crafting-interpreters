module Hlox.Parse (
    Parser,
    ParserError,
    parseProgram,
    parserErrorPretty,
) where

import Hlox.Parse.Parsers (programP)
import Hlox.Parse.Types (Parser, ParserError)
import Hlox.Syntax (Program)

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (errorBundlePretty, parse)

parserErrorPretty :: ParserError -> Text
parserErrorPretty = T.pack . errorBundlePretty

parseProgram :: String -> Text -> Either ParserError Program
parseProgram = parse programP
