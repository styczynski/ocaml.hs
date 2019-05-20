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

run :: Verbosity -> String -> IO String
run v s = let ts = myLLexer s in case pImplementation ts of
          Bad s    -> return $ show s
          Ok  tree -> do
                        putStrLn "Hello"
                        res <- runAST tree emptyEnv
                        return (resultToStr res)