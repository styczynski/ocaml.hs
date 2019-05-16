module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime
import Printer
import AbsSyntax

data Environment = EmptyEnv

type Exec = StateT (RuntimeState) (ReaderT (Environment) (ExceptT String IO))
type ProgramFn = Environment -> Exec RuntimeValue

data Program = Valid ProgramFn | Invalid (String)
data InterpreterState = EmptyState | ValueState

type Eval = StateT (InterpreterState) (ReaderT (Environment) (ExceptT String IO))
eval :: Implementation -> Eval ProgramFn

eval _ = do
  return (\env -> do
    return RUnit)

runInterpretAST tree env = runExceptT (runReaderT (runStateT (eval tree) (EmptyState)) (env))

interpretAST tree env = do
  e <- runInterpretAST tree env
  program <- return (case e of
    Left s -> Invalid s
    Right (e, _) -> Valid e)
  return program

execProgram :: ProgramFn -> Environment -> IO ProgramResult
execProgram prog env = do
  e <- runExceptT (runReaderT (runStateT (prog env) (EmptyRuntimeState)) (env))
  result <- return (case e of
    Left err -> FailedExecution err
    Right (res, state) -> Executed res state)
  return result

runAST :: Implementation -> Environment -> IO ProgramResult
runAST tree env = do
  program <- interpretAST tree env
  result <- (case program of
    Valid p -> execProgram p env
    Invalid s -> return (FailedParse s))
  return result