{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Parser.Common (Parser)
import Parser.Lexer (lexeme, parseIdentifier, spaceConsumer, symbol)
import Syntax (Declaration (..), Expr (..), Program (..), Stmt (..))

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Function ((&))
import Text.Megaparsec (choice, eof, many, optional, single, takeWhileP, try)
import Text.Megaparsec.Char.Lexer qualified as L

-- Program

programP :: Parser Program
programP = spaceConsumer *> (Program <$> many declarationP) <* eof

-- Declaration

declarationP :: Parser Declaration
declarationP =
    choice
        [ varDeclP
        , statementP
        ]

varDeclP :: Parser Declaration
varDeclP =
    VarDecl
        <$> (symbol "var" *> parseIdentifier)
        <*> optional (symbol "=" *> exprP)
        <* symbol ";"

statementP :: Parser Declaration
statementP = Statement <$> stmtP

-- Stmt

stmtP :: Parser Stmt
stmtP =
    choice
        [ printStmtP
        , exprStmtP
        ]

printStmtP :: Parser Stmt
printStmtP = PrintStmt <$> (symbol "print" *> exprP <* symbol ";")

exprStmtP :: Parser Stmt
exprStmtP = ExprStmt <$> (exprP <* symbol ";")

-- Expr

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
        [ nilP
        , literalBooleanP
        , literalNumberP
        , literalStringP
        , identifierP
        , groupingP
        ]

identifierP :: Parser Expr
identifierP = Identifier <$> parseIdentifier

nilP :: Parser Expr
nilP = LiteralNil <$ symbol "nil"

literalBooleanP :: Parser Expr
literalBooleanP = LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")

literalNumberP :: Parser Expr
literalNumberP = LiteralNumber <$> lexeme (try L.float <|> L.decimal)

literalStringP :: Parser Expr
literalStringP = LiteralString <$> lexeme (single '"' *> takeWhileP Nothing (/= '"') <* single '"')

groupingP :: Parser Expr
groupingP = Grouping <$> (symbol "(" *> exprP <* symbol ")")

-- Utilities

binaryExpr :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
binaryExpr operandP binaryConsP = do
    left <- operandP
    rest <- many (flip <$> binaryConsP <*> operandP)
    pure $ foldl' (&) left rest
