module Lib where

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import PrintSyntax
import LexSyntax
import ParSyntax
import SkelSyntax
import AbsSyntax

import Environment
import Runtime
import Interpreter
import Printer

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runWith :: Verbosity -> String -> Environment -> IO ProgramResult
runWith v s env = let ts = myLLexer s in case pImplementation ts of
          Bad s    -> return $ FailedParse $ show s
          Ok  tree -> do
                        res <- runAST tree env
                        return res

run :: Verbosity -> String -> IO ProgramResult
run v s = runWith v s emptyEnv