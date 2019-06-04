module Interpreter where

import InterpreterDefinitions
import Environment
import Runtime
import AbsSyntax

runAST :: Implementation -> Environment -> IO ExecutionResult
runAST _ _  = do
  return $ FailedParse "LOL!"