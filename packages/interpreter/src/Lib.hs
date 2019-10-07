{-|
Module      : Lib
Description : Base interpreter entrypoint
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module is the top one, it provides handful ways to run the parser, interpreter and typechecker.
-}
module Lib where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Syntax.Base

import Runtime.Runtime
import Runtime.Environment

import Interop.Startup
import Interpreter.Definitions
import Interpreter.Interpreter

import Inference.Inferencer
import Inference.TypingEnvironment
import Inference.Errors
import qualified Inference.Types as Types
import Inference.Syntax

type Verbosity = Int

runTIWith :: Verbosity -> String -> Environment -> IO (Either TypeError (Types.Scheme, Environment))
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
    (Right ((Types.Scheme _ inferType), initEnv)) -> do
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

runWithPrelude :: Verbosity -> String -> String -> IO ExecutionResult
runWithPrelude v preludeFile input = do
    initEnv0 <- runInitEmpty
    files <- getDirectoryContents "."
    putStrLn "RUN WITH PRELUDE DEBUG_INFO_OCAMLHS:"
    print files
    stdlibStr <- readFile preludeFile
    (Executed _ _ initEnv) <- runWith v stdlibStr initEnv0
    result <- runWith v input initEnv
    return $ result

extractExecutionErrors :: IO ExecutionResult -> IO (Maybe String)
extractExecutionErrors e = do
    e >>= \result -> case result of
        Executed _ _ _ -> return $ Nothing
        FailedTypechecking s -> return $ Just $ typeErrorToStr s
        FailedExecution s -> return $ Just s
        FailedParse s -> return $ Just s
