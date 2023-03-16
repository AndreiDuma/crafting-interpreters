{-# LANGUAGE OverloadedStrings #-}

module Hlox.Parse.Parsers where

import Hlox.Parse.Lexer (identifierP, lexeme, numberP, spaceP, symbol)
import Hlox.Parse.Types (Parser)
import Hlox.Syntax

import Data.Foldable (foldl')
import Data.Function ((&))
import Hlox.Syntax (LiteralNil)
import Text.Megaparsec (choice, eof, many, optional, single, takeWhileP, try, (<|>))

-- -- Program

-- programP :: Parser p
-- programP = spaceP *> (Program <$> many declarationP) <* eof

-- -- Declarations

-- declarationP :: Parser d
-- declarationP =
--     choice
--         [ varDeclP
--         , statementP
--         ]

-- varDeclP :: Parser d
-- varDeclP =
--     VarDecl
--         <$> (symbol "var" *> identifierP)
--         <*> optional (symbol "=" *> exprP)
--         <* symbol ";"

-- statementP :: Parser d
-- statementP = StmtDecl <$> stmtP

-- -- Statements

-- stmtP :: Parser s
-- stmtP =
--     choice
--         [ blockStmtP
--         , ifStmtP
--         , whileStmt
--         , printStmtP
--         , exprStmtP
--         ]

-- blockStmtP :: Parser s
-- blockStmtP = BlockStmt <$> (symbol "{" *> many declarationP <* symbol "}")

-- ifStmtP :: Parser s
-- ifStmtP =
--     IfStmt
--         <$> (symbol "if" *> symbol "(" *> exprP <* symbol ")")
--         <*> stmtP
--         <*> optional (symbol "else" *> stmtP)

-- whileStmt :: Parser s
-- whileStmt = WhileStmt <$> (symbol "while" *> symbol "(" *> exprP <* symbol ")") <*> stmtP

-- printStmtP :: Parser s
-- printStmtP = PrintStmt <$> (symbol "print" *> exprP <* symbol ";")

-- exprStmtP :: Parser s
-- exprStmtP = ExprStmt <$> (exprP <* symbol ";")

-- -- Expressions

-- exprP :: Parser e
-- exprP = assignmentP

-- assignmentP :: Parser e
-- assignmentP =
--     choice
--         [ try $ Assignment <$> identifierP <*> (symbol "=" *> exprP)
--         , logicOrP
--         ]

-- logicOrP :: Parser e
-- logicOrP = binaryExpr logicAndP (Or <$ symbol "or")

-- logicAndP :: Parser e
-- logicAndP = binaryExpr equalityP (And <$ symbol "and")

-- equalityP :: Parser e
-- equalityP = binaryExpr comparisonP (Equal <$ symbol "==" <|> NotEqual <$ symbol "!=")

-- comparisonP :: Parser e
-- comparisonP =
--     binaryExpr termP $
--         choice
--             [ GreaterOrEqual <$ symbol ">="
--             , Greater <$ symbol ">"
--             , Less <$ symbol "<"
--             , LessOrEqual <$ symbol "<="
--             ]

-- termP :: Parser e
-- termP = binaryExpr factorP (Plus <$ symbol "+" <|> Minus <$ symbol "-")

-- factorP :: Parser e
-- factorP = binaryExpr unaryP (Star <$ symbol "*" <|> Slash <$ symbol "/")

-- unaryP :: Parser e
-- unaryP =
--     choice
--         [ Not <$> (symbol "!" *> unaryP)
--         , Negate <$> (symbol "-" *> unaryP)
--         , primaryP
--         ]

-- primaryP :: Parser e
-- primaryP =
--     choice
--         [ LiteralNil <$ symbol "nil"
--         , LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")
--         , LiteralNumber <$> numberP
--         , LiteralString <$> lexeme (single '"' *> takeWhileP Nothing (/= '"') <* single '"')
--         , Identifier <$> identifierP
--         , Grouping <$> (symbol "(" *> exprP <* symbol ")")
--         ]

-- -- Utilities

-- binaryExpr :: Parser e -> Parser (e -> e -> e) -> Parser e
-- binaryExpr operandP binaryConsP = do
--     left <- operandP
--     rest <- many (flip <$> binaryConsP <*> operandP)
--     pure $ foldl' (&) left rest

------------------------------------------------------------------------------

-- primaryP :: Parser e
-- primaryP =
--     choice
--         [ LiteralNil <$ symbol "nil"
--         , LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")
--         , LiteralNumber <$> numberP
--         , LiteralString <$> lexeme (single '"' *> takeWhileP Nothing (/= '"') <* single '"')
--         , Identifier <$> identifierP
--         -- , Grouping <$> (symbol "(" *> exprP <* symbol ")")
--         ]

class Expr e => Parse e where
    parse :: Parser e

instance Parse LiteralNil where
    parse :: Parser LiteralNil
    parse = LiteralNil <$ symbol "nil"

instance Parse LiteralBoolean where
    parse :: Parser LiteralBoolean
    parse = LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")

primaryP =
    choice
        [ LiteralNil <$ symbol "nil"
        , LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")
        ]
