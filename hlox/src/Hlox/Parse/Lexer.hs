{-# LANGUAGE OverloadedStrings #-}

module Hlox.Parse.Lexer where

import Hlox.Parse.Types (Parser)

import Data.Char (isAscii, isDigit, isLetter)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (empty, satisfy, single, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

spaceP :: Parser ()
spaceP = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: Text -> Parser Text
symbol = L.symbol spaceP

numberP :: Parser Double
numberP = lexeme (try L.float <|> L.decimal)

stringP :: Parser Text
stringP = lexeme (single '"' *> takeWhileP Nothing (/= '"') <* single '"')

identifierP :: Parser Text
identifierP = do
    first <- satisfy isAlpha
    rest <- takeWhileP Nothing isAlphaOrDigit
    lexeme . pure $ T.cons first rest
  where
    isAlpha c = isAscii c && isLetter c || c == '_'
    isAlphaOrDigit c = isAlpha c || isDigit c
