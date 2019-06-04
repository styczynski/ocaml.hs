module Interpreter where

import InterpreterDefinitions
import InterpreterExpressions
import Environment
import Runtime
import AbsSyntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

exec :: Implementation -> Exec RuntimeValue
exec (IRoot implPhrase) = execPhrase implPhrase

execPhrase :: ImplPhrase -> Exec RuntimeValue
execPhrase (IPhrase expr) = execComplexExpression expr

runAST :: Implementation -> Environment -> IO ExecutionResult
runAST tree env  = do
  r <- runExceptT (runReaderT (runStateT (exec tree) (env)) (env))
  result <- return (case r of
      Left err -> FailedExecution err
      Right (res, state) -> Executed res state)
  return result