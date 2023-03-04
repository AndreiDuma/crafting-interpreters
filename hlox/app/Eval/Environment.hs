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
import Eval.Result (Result)

newtype Environment = Environment
    { getEnvironment :: Map Text Result
    }

empty :: Environment
empty = Environment M.empty

getVar :: Text -> Environment -> Maybe Result
getVar var = M.lookup var . getEnvironment

defineVar :: Text -> Result -> Environment -> Environment
defineVar var value = Environment . M.insert var value . getEnvironment

assignVar :: Text -> Result -> Environment -> Maybe Environment
assignVar var value env =
    if M.member var $ getEnvironment env
        then Just (Environment $ M.insert var value $ getEnvironment env)
        else Nothing
