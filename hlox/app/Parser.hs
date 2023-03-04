{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char (isAscii, isDigit, isLetter)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Syntax (
    Declaration (..),
    Expr (..),
    Program (..),
    Stmt (..),
 )
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

binaryExpr :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
binaryExpr pOperand pBinaryCons = do
    left <- pOperand
    rest <- many (flip <$> pBinaryCons <*> pOperand)
    pure $ foldl' (&) left rest

-- Program

pProgram :: Parser Program
pProgram = spaceConsumer *> (Program <$> many pDeclaration) <* eof

-- Declaration

pDeclaration :: Parser Declaration
pDeclaration =
    choice
        [ pVarDecl
        , pStatement
        ]

pVarDecl :: Parser Declaration
pVarDecl =
    VarDecl
        <$> (symbol "var" *> parseIdentifier)
        <*> optional (symbol "=" *> pExpr)
        <* symbol ";"

pStatement :: Parser Declaration
pStatement = Statement <$> pStmt

-- Stmt

pStmt :: Parser Stmt
pStmt =
    choice
        [ pPrintStmt
        , pExprStmt
        ]

pPrintStmt :: Parser Stmt
pPrintStmt = PrintStmt <$> (symbol "print" *> pExpr <* symbol ";")

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> (pExpr <* symbol ";")

-- Expr

pExpr :: Parser Expr
pExpr = pLogicOr

pLogicOr :: Parser Expr
pLogicOr = binaryExpr pLogicAnd (Or <$ symbol "or")

pLogicAnd :: Parser Expr
pLogicAnd = binaryExpr pEquality (And <$ symbol "and")

pEquality :: Parser Expr
pEquality = binaryExpr pComparison (Equal <$ symbol "==" <|> NotEqual <$ symbol "!=")

pComparison :: Parser Expr
pComparison =
    binaryExpr pTerm $
        choice
            [ Greater <$ symbol ">"
            , GreaterOrEqual <$ symbol ">="
            , Less <$ symbol "<"
            , LessOrEqual <$ symbol "<="
            ]

pTerm :: Parser Expr
pTerm = binaryExpr pFactor (Plus <$ symbol "+" <|> Minus <$ symbol "-")

pFactor :: Parser Expr
pFactor = binaryExpr pUnary (Star <$ symbol "*" <|> Slash <$ symbol "/")

pUnary :: Parser Expr
pUnary =
    choice
        [ Not <$> (symbol "!" *> pUnary)
        , Negate <$> (symbol "-" *> pUnary)
        , pPrimary
        ]

pPrimary :: Parser Expr
pPrimary =
    choice
        [ pNil
        , pLiteralBoolean
        , pLiteralNumber
        , pLiteralString
        , pIdentifier
        , pGrouping
        ]

pIdentifier :: Parser Expr
pIdentifier = Identifier <$> parseIdentifier

pNil :: Parser Expr
pNil = LiteralNil <$ symbol "nil"

pLiteralBoolean :: Parser Expr
pLiteralBoolean = LiteralBoolean <$> (True <$ symbol "true" <|> False <$ symbol "false")

pLiteralNumber :: Parser Expr
pLiteralNumber = LiteralNumber <$> lexeme (try L.float <|> L.decimal)

pLiteralString :: Parser Expr
pLiteralString = LiteralString <$> lexeme (single '"' *> takeWhileP Nothing (/= '"') <* single '"')

pGrouping :: Parser Expr
pGrouping = Grouping <$> (symbol "(" *> pExpr <* symbol ")")

-- Lexing

parseIdentifier :: Parser Text
parseIdentifier = do
    first <- satisfy isAlpha
    rest <- takeWhileP Nothing isAlphaOrDigit
    lexeme . pure $ T.cons first rest
  where
    isAlpha c = isAscii c && isLetter c || c == '_'
    isAlphaOrDigit c = isAlpha c || isDigit c
