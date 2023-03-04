{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import Parser.Common (Parser)

import Data.Char (isAscii, isDigit, isLetter)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (empty, satisfy, takeWhileP)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parseIdentifier :: Parser Text
parseIdentifier = do
    first <- satisfy isAlpha
    rest <- takeWhileP Nothing isAlphaOrDigit
    lexeme . pure $ T.cons first rest
  where
    isAlpha c = isAscii c && isLetter c || c == '_'
    isAlphaOrDigit c = isAlpha c || isDigit c
