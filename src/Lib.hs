module Lib (
  run,
  execContents
) where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexSyntax
import ParSyntax
import SkelSyntax
import AbsSyntax

import Runtime
import Interpreter
import Printer

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: Verbosity -> ParseFun Implementation -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Implementation -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          res <- runAST tree EmptyEnv
                          putStrLn (resultToStr res)
                          exitSuccess

execContents = getContents >>= run 2 pImplementation