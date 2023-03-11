{-# LANGUAGE OverloadedStrings #-}

module Parser.Declaration (declarationP) where

import Parser.Expression (exprP)
import Parser.Lexer (identifierP, symbol)
import Parser.Statement (stmtP)
import Parser.Type (Parser)
import Syntax (Declaration (..))

import Text.Megaparsec (choice, optional)

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
