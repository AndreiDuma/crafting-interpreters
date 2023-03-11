module Parser.Type (
    Parser,
    ParserError,
) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void
