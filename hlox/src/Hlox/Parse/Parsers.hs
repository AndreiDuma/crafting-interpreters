{-# LANGUAGE OverloadedStrings #-}

module Hlox.Parse.Parsers where

import Hlox.Parse.Lexer (identifierP, lexeme, numberP, spaceP, symbol)
import Hlox.Parse.Types (Parser)
import Hlox.Syntax (Decl (..), Expr (..), Program (..), Stmt (..))

import Data.Foldable (foldl')
import Data.Function ((&))
import Text.Megaparsec (choice, eof, many, optional, single, takeWhileP, try, (<|>))

-- Program

programP :: Parser Program
programP = spaceP *> (Program <$> many declarationP) <* eof

-- Declarations

declarationP :: Parser Decl
declarationP =
    choice
        [ varDeclP
        , statementP
        ]

varDeclP :: Parser Decl
varDeclP =
    VarDecl
        <$> (symbol "var" *> identifierP)
        <*> optional (symbol "=" *> exprP)
        <* symbol ";"

statementP :: Parser Decl
statementP = StmtDecl <$> stmtP

-- Statements

stmtP :: Parser Stmt
stmtP =
    choice
        [ blockStmtP
        , ifStmtP
        , printStmtP
        , exprStmtP
        ]

blockStmtP :: Parser Stmt
blockStmtP = BlockStmt <$> (symbol "{" *> many declarationP <* symbol "}")

ifStmtP :: Parser Stmt
ifStmtP =
    IfStmt
        <$> (symbol "if" *> symbol "(" *> exprP <* symbol ")")
        <*> stmtP
        <*> optional (symbol "else" *> stmtP)

printStmtP :: Parser Stmt
printStmtP = PrintStmt <$> (symbol "print" *> exprP <* symbol ";")

exprStmtP :: Parser Stmt
exprStmtP = ExprStmt <$> (exprP <* symbol ";")

-- Expressions

exprP :: Parser Expr
exprP = assignmentP

assignmentP :: Parser Expr
assignmentP =
    choice
        [ try $ Assignment <$> identifierP <*> (symbol "=" *> exprP)
        , logicOrP
        ]

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
            [ GreaterOrEqual <$ symbol ">="
            , Greater <$ symbol ">"
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
