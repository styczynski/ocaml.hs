module Lib where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import PrintSyntax
import LexSyntax
import ParSyntax
import SkelSyntax
import AbsSyntax
import ErrM

import Startup
import Runtime
import Environment
import InterpreterDefinitions
import Interpreter

import Infer
import Syntax
import Type
import Env
import Environment

type Verbosity = Int

runTIWith :: Verbosity -> String -> Environment -> Either TypeError (Scheme, Environment)
runTIWith v s e = let state = getTypesState e in let env = getTypesEnv e in let ts = myLexer s in case pImplementation ts of
          Bad s    -> Left $ Debug $ show s
          Ok  tree ->
            case inferAST env state tree of
              (Left e) -> Left e
              (Right (r, env, state)) -> Right (r, (setTypesState state (setTypesEnv env e)))
              --(Left mes) -> putStrLn $ show mes

runInit :: Environment -> IO Environment
runInit env = do
  result <- runFn interpreterStartupFn env
  case result of
    (Executed _ newEnv) -> return newEnv
    _ -> return env

runInitEmpty :: IO Environment
runInitEmpty = runInit emptyEnv

runWith :: Verbosity -> String -> Environment -> IO ExecutionResult
runWith v s env = let ts = myLexer s in case pImplementation ts of
          Bad s    -> return $ FailedParse $ show s
          Ok  tree -> do
                        res <- runAST tree env
                        return res

run :: Verbosity -> String -> IO ExecutionResult
run v s = do
  initEnv <- runInitEmpty
  runWith v s initEnv