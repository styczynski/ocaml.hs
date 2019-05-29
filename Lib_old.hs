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

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v

evaluate :: Verbosity -> String -> IO ProgramResult
evaluate v s = let ts = myLLexer s in case pImplementation ts of
          Bad s    -> return $ FailedParse s
          Ok  tree -> runAST tree emptyEnv

run :: Verbosity -> String -> IO ()
run v s = let ts = myLLexer s in case pImplementation ts of
           Bad s    -> do hPutStrLn stderr "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          hPutStrLn stderr s
                          exitFailure
           Ok  tree -> do
                          res <- runAST tree emptyEnv
                          putStrLn (resultToStr res)
                          exitSuccess

prettify :: Verbosity -> String -> IO String
prettify v s = let ts = myLLexer s in case pImplementation ts of
            Bad s    -> do hPutStrLn stderr "\nParse              Failed...\n"
                           putStrV v $ show ts
                           hPutStrLn stderr s
                           exitFailure
            Ok  tree -> do
                           return (printTree tree)

generateHS :: Verbosity -> String -> IO String
generateHS v s = let ts = myLLexer s in case pImplementation ts of
            Bad s    -> do hPutStrLn stderr "\nParse              Failed...\n"
                           putStrV v $ show ts
                           hPutStrLn stderr s
                           exitFailure
            Ok  tree -> do
                           res <- genHSAST tree emptyEnv
                           return res

generateHSFromContents v = getContents >>= generateHS v
prettifyContents v = getContents >>= prettify v
execContents v = getContents >>= run v