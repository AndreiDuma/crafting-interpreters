{-# LANGUAGE OverloadedStrings #-}

module Parser.Statement (stmtP) where

import Parser.Expression (exprP)
import Parser.Lexer (symbol)
import Parser.Type (Parser)
import Syntax (Stmt (..))

import Text.Megaparsec (choice)

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
