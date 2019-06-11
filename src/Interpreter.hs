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

import Startup

exec :: Implementation -> Exec (RuntimeValue, Environment)
exec (IRoot implPhrases) = do
  rootEnv <- ask
  foldM (\(_,env) phrase -> do
     local (\_ -> env) $ execPhrase phrase) (REmpty,rootEnv) implPhrases

execPhrase :: ImplPhrase -> Exec (RuntimeValue, Environment)
execPhrase (IPhrase expr) = execComplexExpression expr
execPhrase (IDefType typeDef) = execTypeDef typeDef

runAST :: Implementation -> Environment -> IO ExecutionResult
runAST tree env  = do
  r <- runExceptT (runReaderT (runStateT (exec tree) (InterpreterState { lastNode = "", lastNodeDetail = "", trace = [], globalExportEnv = Nothing })) (env))
  result <- return (case r of
      Left err -> FailedExecution err
      Right ((res, env), InterpreterState { globalExportEnv = Nothing }) -> Executed res env
      Right ((res, _), InterpreterState { globalExportEnv = (Just env) }) -> Executed res env)
  return result

runFn :: (Exec (RuntimeValue, Environment)) -> Environment -> IO ExecutionResult
runFn fn env = do
  r <- runExceptT (runReaderT (runStateT (fn) (InterpreterState { lastNode = "", lastNodeDetail = "", trace = [], globalExportEnv = Nothing })) (env))
  result <- return (case r of
      Left err -> FailedExecution err
      Right ((res, env), InterpreterState { globalExportEnv = Nothing } ) -> Executed res env
      Right ((res, _), InterpreterState { globalExportEnv = (Just env) }) -> Executed res env)
  return result
