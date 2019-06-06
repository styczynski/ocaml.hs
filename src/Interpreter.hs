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

exec :: Implementation -> Exec (RuntimeValue, Environment)
exec (IRoot implPhrase) = execPhrase implPhrase

execPhrase :: ImplPhrase -> Exec (RuntimeValue, Environment)
execPhrase (IPhrase expr) = execComplexExpression expr

runAST :: Implementation -> Environment -> IO ExecutionResult
runAST tree env  = do
  r <- runExceptT (runReaderT (runStateT (exec tree) (InterpreterState { lastNode = "", lastNodeDetail = "", trace = [] })) (env))
  result <- return (case r of
      Left err -> FailedExecution err
      Right ((res, env), _) -> Executed res env)
  return result