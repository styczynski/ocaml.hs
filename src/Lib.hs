module Lib where

import PrintSyntax
import LexSyntax
import ParSyntax
import SkelSyntax
import AbsSyntax
import ErrM

import Runtime
import Environment
import InterpreterDefinitions
import Interpreter

type Verbosity = Int

runWith :: Verbosity -> String -> Environment -> IO ExecutionResult
runWith v s env = let ts = myLexer s in case pImplementation ts of
          Bad s    -> return $ FailedParse $ show s
          Ok  tree -> do
                        res <- runAST tree env
                        return res

run :: Verbosity -> String -> IO ExecutionResult
run v s = runWith v s emptyEnv