{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import Parser.Type (Parser)
import Parser.Lexer (identifierP, lexeme, numberP, symbol)
import Syntax (Expr (..))

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Function ((&))
import Text.Megaparsec (choice, many, single, takeWhileP)

exprP :: Parser Expr
exprP = logicOrP

logicOrP :: Parser Expr
logicOrP = binaryExpr logicAndP (Or <$ symbol "or")

logicAndP :: Parser Expr
logicAndP = binaryExpr equalityP (And <$ symbol "and")

equalityP :: Parser Expr
equalityP = binaryExpr comparisonP (Equal <$ symbol "==" <|> NotEqual <$ symbol "!=")

comparisonP :: Parser Expr
comparisonP =
    binaryExpr termP $
        choice
            [ Greater <$ symbol ">"
            , GreaterOrEqual <$ symbol ">="
            , Less <$ symbol "<"
            , LessOrEqual <$ symbol "<="
            ]

termP :: Parser Expr
termP = binaryExpr factorP (Plus <$ symbol "+" <|> Minus <$ symbol "-")

factorP :: Parser Expr
factorP = binaryExpr unaryP (Star <$ symbol "*" <|> Slash <$ symbol "/")

unaryP :: Parser Expr
unaryP =
    choice
        [ Not <$> (symbol "!" *> unaryP)
        , Negate <$> (symbol "-" *> unaryP)
        , primaryP
        ]

primaryP :: Parser Expr
primaryP =
    choice
        [ LiteralNil <$ symbol "nil"
        , LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")
        , LiteralNumber <$> numberP
        , LiteralString <$> lexeme (single '"' *> takeWhileP Nothing (/= '"') <* single '"')
        , Identifier <$> identifierP
        , Grouping <$> (symbol "(" *> exprP <* symbol ")")
        ]

-- Utilities

binaryExpr :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
binaryExpr operandP binaryConsP = do
    left <- operandP
    rest <- many (flip <$> binaryConsP <*> operandP)
    pure $ foldl' (&) left rest
