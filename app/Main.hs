module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Lib

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


runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= runBlock v

runBlock :: Verbosity -> String -> IO ()
runBlock v s = let ts = myLLexer s in case pImplementation ts of
           Bad s    -> do hPutStrLn stderr "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          hPutStrLn stderr s
                          exitFailure
           Ok  tree -> do
                          res <- runAST tree emptyEnv
                          putStrLn (resultToStr res)
                          exitSuccess

runPrettify :: Verbosity -> String -> IO String
runPrettify v s = let ts = myLLexer s in case pImplementation ts of
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
prettifyContents v = getContents >>= runPrettify v
execContents v = getContents >>= runBlock v

data MainArgs = MainArgs
  { verbosity :: Int
  , prettify :: Bool
  , generatehs :: Bool }

parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> switch
    ( long "prettify"
      <> short 'p'
      <> help "Do not interpret anything, only prettify the code and print it." )
  <*> switch
      ( long "generatehs"
        <> short 'g'
        <> help "Do not interpret anything just parse macros and generate Haskell code." )

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Tiny Ocaml interprter for Haskell"
      <> header "Piotr Styczynski 2019" )

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity prettify generatehs) = case (verbosity, prettify, generatehs) of
  (v, True, False) -> (prettifyContents v) >>= putStrLn
  (v, False, False) -> execContents v
  (v, _, True) -> (generateHSFromContents v) >>= putStrLn
mainEntry _ = return ()