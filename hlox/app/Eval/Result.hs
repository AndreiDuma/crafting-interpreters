module Eval.Result (
    Result (..),
) where

import Data.Text (Text)

-- TODO: rename to Value
data Result = Nil | Boolean Bool | Number Double | String Text
    deriving (Eq, Show)
