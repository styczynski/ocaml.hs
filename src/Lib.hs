module Lib where

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

type Verbosity = Int

runTIWith :: Verbosity -> String -> IO ()
runTIWith v s = let ts = myLexer s in case pImplementation ts of
          Bad s    -> putStrLn (show s)
          Ok  tree -> do
            r <- return $ inferAST empty tree
            case r of
              (Right r) -> putStrLn (show r)
              (Left mes) -> putStrLn $ show mes

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