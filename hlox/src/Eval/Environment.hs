module Eval.Environment (
    Environment,
    empty,
    getVar,
    defineVar,
    assignVar,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Eval.Value (Value)

newtype Environment = Environment
    { getEnvironment :: Map Text Value
    }

empty :: Environment
empty = Environment M.empty

getVar :: Text -> Environment -> Maybe Value
getVar var = M.lookup var . getEnvironment

defineVar :: Text -> Value -> Environment -> Environment
defineVar var value = Environment . M.insert var value . getEnvironment

assignVar :: Text -> Value -> Environment -> Maybe Environment
assignVar var value env =
    if M.member var $ getEnvironment env
        then Just (Environment $ M.insert var value $ getEnvironment env)
        else Nothing
