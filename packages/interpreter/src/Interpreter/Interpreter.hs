{-|
Module      : Interpreter.Interpreter
Description : Base interpreter code
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This file provides base for executing the code.
-}
module Interpreter.Interpreter where

import           Runtime.Environment
import           Runtime.Runtime

import           Interpreter.Definitions
import           Interpreter.Expressions

import           Syntax.Base

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Foldable

import           Interop.Startup

import qualified Inference.Types               as Types

exec :: Implementation -> Exec (RuntimeValue, Environment)
exec (IRoot implCores) = do
  rootEnv <- ask
  foldlM
    (\(_, env) core -> do
      local (\_ -> env) $ execCoreImpl core
    )
    (REmpty, rootEnv)
    implCores

execCoreImpl :: ImplementationCore -> Exec (RuntimeValue, Environment)
execCoreImpl (IRootExpr expr       ) = execComplexExpression expr
execCoreImpl (IRootDef  implPhrases) = do
  rootEnv <- ask
  foldlM
    (\(_, env) phrase -> do
      local (\_ -> env) $ execPhrase phrase
    )
    (REmpty, rootEnv)
    implPhrases

runAST :: Implementation -> Environment -> IO ExecutionResult
runAST tree env = do
  r <- runExceptT
    (runReaderT
      (runStateT
        (exec tree)
        (InterpreterState { lastNode        = ""
                          , lastNodeDetail  = ""
                          , trace           = []
                          , globalExportEnv = Nothing
                          }
        )
      )
      (env)
    )
  result <- return
    (case r of
      Left err -> FailedExecution err
      Right ((res, env), InterpreterState { globalExportEnv = Nothing }) ->
        Executed res Types.TypeUnit env
      Right ((res, _), InterpreterState { globalExportEnv = (Just env) }) ->
        Executed res Types.TypeUnit env
    )
  return result

runFn :: (Exec (RuntimeValue, Environment)) -> Environment -> IO ExecutionResult
runFn fn env = do
  r <- runExceptT
    (runReaderT
      (runStateT
        (fn)
        (InterpreterState { lastNode        = ""
                          , lastNodeDetail  = ""
                          , trace           = []
                          , globalExportEnv = Nothing
                          }
        )
      )
      (env)
    )
  result <- return
    (case r of
      Left err -> FailedExecution err
      Right ((res, env), InterpreterState { globalExportEnv = Nothing }) ->
        Executed res Types.TypeUnit env
      Right ((res, _), InterpreterState { globalExportEnv = (Just env) }) ->
        Executed res Types.TypeUnit env
    )
  return result
