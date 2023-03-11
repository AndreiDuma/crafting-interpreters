module Parser.Program (programP) where

import Parser.Declaration (declarationP)
import Parser.Lexer (spaceP)
import Parser.Type (Parser)
import Syntax (Program (..))

import Text.Megaparsec (eof, many)

programP :: Parser Program
programP = spaceP *> (Program <$> many declarationP) <* eof
