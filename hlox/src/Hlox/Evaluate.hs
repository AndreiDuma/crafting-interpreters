module Hlox.Evaluate (
    evaluateProgram,
) where

import Hlox.Evaluate.Environment (global)
import Hlox.Evaluate.Eval (evalEval)
import Hlox.Evaluate.Evaluators (programEval)
import Hlox.Syntax (Program)

import Data.Text (Text)

evaluateProgram :: Program -> IO (Either Text ())
evaluateProgram program = evalEval (programEval program) global
