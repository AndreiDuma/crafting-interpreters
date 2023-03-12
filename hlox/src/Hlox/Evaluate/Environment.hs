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

-- | An environment is a stack of scopes.
data Environment = Environment
    { enclosing :: Maybe Environment
    , variables :: Map Text Value
    }
    deriving (Eq, Show)

-- | Create a new global scope.
global :: Environment
global = Environment Nothing M.empty

-- | Create a new local scope.
local :: Environment -> Environment
local env = Environment (Just env) M.empty

-- | Get a variable from the current scope or any enclosing scope.
getVariable :: Text -> Environment -> Maybe Value
getVariable name Environment{enclosing, variables} = M.lookup name variables <|> (enclosing >>= getVariable name)

-- | Define a variable in the current scope.
defineVariable :: Text -> Value -> Environment -> Environment
defineVariable name value env@Environment{variables} = env{variables = M.insert name value variables}

-- | Assign a variable in the current scope or any enclosing scope.
assignVariable :: Text -> Value -> Environment -> Maybe Environment
assignVariable name value env@Environment{enclosing, variables} =
    if M.member name variables
        then -- if variable is defined in current scope, assign it
            pure env{variables = M.insert name value variables}
        else -- otherwise, try to assign it in enclosing scope
        case assignVariable name value <$> enclosing of
            Just env' -> pure env{enclosing = env'}
            Nothing -> Nothing
