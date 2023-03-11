{-# LANGUAGE OverloadedStrings #-}

module Hlox.Parser.Parsers where

import Hlox.Parser.Lexer (identifierP, lexeme, numberP, spaceP, symbol)
import Hlox.Parser.Type (Parser)
import Hlox.Syntax (Declaration (..), Expr (..), Program (..), Stmt (..))

import Data.Foldable (foldl')
import Data.Function ((&))
import Text.Megaparsec (choice, eof, many, optional, single, takeWhileP, (<|>))

programP :: Parser Program
programP = spaceP *> (Program <$> many declarationP) <* eof

-- Declarations

declarationP :: Parser Declaration
declarationP =
    choice
        [ varDeclP
        , statementP
        ]

varDeclP :: Parser Declaration
varDeclP =
    VarDecl
        <$> (symbol "var" *> identifierP)
        <*> optional (symbol "=" *> exprP)
        <* symbol ";"

statementP :: Parser Declaration
statementP = Statement <$> stmtP

-- Statements

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

-- Expressions

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
