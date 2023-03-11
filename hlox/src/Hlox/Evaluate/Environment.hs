module Hlox.Evaluate.Environment (
    Environment,
    empty,
    getVariable,
    defineVariable,
    assignVariable,
) where

import Hlox.Evaluate.Value (Value)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)

newtype Environment = Environment
    { getEnvironment :: Map Text Value
    }

empty :: Environment
empty = Environment M.empty

getVariable :: Text -> Environment -> Maybe Value
getVariable name = M.lookup name . getEnvironment

defineVariable :: Text -> Value -> Environment -> Environment
defineVariable name value = Environment . M.insert name value . getEnvironment

assignVariable :: Text -> Value -> Environment -> Maybe Environment
assignVariable name value env =
    if M.member name $ getEnvironment env
        then Just (Environment $ M.insert name value $ getEnvironment env)
        else Nothing
