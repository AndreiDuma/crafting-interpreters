module Hlox.Evaluate (
    evaluateProgram,
) where

import Hlox.Evaluate.Eval (runEval)
import Hlox.Evaluate.Evaluators (programEval)
import Hlox.Syntax (Program)

import Data.Text (Text)

evaluateProgram :: Program -> IO (Either Text ())
evaluateProgram = runEval . programEval
