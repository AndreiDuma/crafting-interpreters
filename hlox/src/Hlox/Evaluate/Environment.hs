module Hlox.Evaluate.Environment (
    Environment (enclosing),
    global,
    local,
    getVariable,
    defineVariable,
    assignVariable,
) where

import Hlox.Evaluate.Value (Value)

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)

data Environment = Environment
    { enclosing :: Maybe Environment
    , get :: Map Text Value
    }

global :: Environment
global = Environment Nothing M.empty

local :: Environment -> Environment
local env = Environment (Just env) M.empty

getVariable :: Text -> Environment -> Maybe Value
getVariable name env = M.lookup name (get env) <|> (enclosing env >>= getVariable name)

defineVariable :: Text -> Value -> Environment -> Environment
defineVariable name value env = Environment (enclosing env) (M.insert name value $ get env)

assignVariable :: Text -> Value -> Environment -> Maybe Environment
assignVariable name value env =
    if M.member name (get env)
        then pure $ Environment (enclosing env) (M.insert name value $ get env)
        else enclosing env >>= assignVariable name value
