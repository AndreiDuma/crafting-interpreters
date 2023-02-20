{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

-- parse
-- parseTest

-- label, <?>

-- token
--   satisfy
--   single, char

-- tokens
--   chunk, string

-- eof
-- newline, eol
-- space, space1 (any white space)
-- hspace, hspace1 (only "horizontal" white space)

-- letterChar
-- digitChar, numberChar
-- alphaNumChar
-- asciiChar

-- many

-- LEXER
-- space (comment-aware space consumer)
-- lexeme
-- symbol

test :: IO ()
test = do
    let
        parser = chunk "ab" :: Parser Text
        input = "abc"
    parseTest parser input
