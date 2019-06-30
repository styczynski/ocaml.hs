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

import Inference.Inferencer
import Inference.Env
import qualified Inference.Type as Type
import Inference.Syntax

type Verbosity = Int

runTIWith :: Verbosity -> String -> Environment -> IO (Either TypeError (Type.Scheme, Environment))
runTIWith v s e = let state = getTypesState e in let env = getTypesEnv e in let ts = myLexer s in case pImplementation ts of
          Bad s    -> return $ Left $ Debug EmptyPayload $ s
          Ok  tree -> do
            r <- inferAST env state tree
            case r of
              (Left e) -> return $ Left e
              (Right (r, env, state)) -> return $ Right (r, (setTypesState state (setTypesEnv env e)))
              --(Left mes) -> putStrLn $ show mes

runInit :: Environment -> IO Environment
runInit env = do
  result <- runFn interpreterStartupFn env
  case result of
    (Executed _ _ newEnv) -> return newEnv
    _ -> return env

runInitEmpty :: IO Environment
runInitEmpty = runInit emptyEnv

runWith :: Verbosity -> String -> Environment -> IO ExecutionResult
runWith v s env = do
  i <- runTIWith v s env
  (case i of
    (Left e) -> return $ FailedTypechecking e
    (Right ((Type.Forall _ inferType), initEnv)) -> do
      r <- runIWith v s initEnv
      return $ case r of
        (Executed v _ env) -> Executed v inferType env
        other -> other)

runIWith :: Verbosity -> String -> Environment -> IO ExecutionResult
runIWith v s env = let ts = myLexer s in case pImplementation ts of
          Bad s    -> return $ FailedParse $ show s
          Ok  tree -> do
                        res <- runAST tree env
                        return res

run :: Verbosity -> String -> IO ExecutionResult
run v s = do
  initEnv <- runInitEmpty
  runWith v s initEnv