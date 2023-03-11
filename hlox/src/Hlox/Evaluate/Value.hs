module Hlox.Evaluate.Value (
    Value (..),
) where

import Data.Text (Text)

data Value = Nil | Boolean Bool | Number Double | String Text
    deriving (Eq, Show)
