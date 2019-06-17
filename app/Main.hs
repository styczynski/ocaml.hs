module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Lib
import Runtime
import InterpreterDefinitions
import Type

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= runBlock v

runBlockI :: Verbosity -> String -> IO ()
runBlockI v s = do
  initEnv0 <- runInitEmpty
  stdlibStr <- readFile "./init/init.ml"
  (Executed _ _ initEnv) <- runWith v stdlibStr initEnv0
  result <- runWith v s initEnv
  case result of
     FailedTypechecking s -> do
                    hPutStrLn stderr $ show s
                    exitFailure
     FailedExecution s -> do
                    hPutStrLn stderr s
                    exitFailure
     FailedParse s  -> do
                    hPutStrLn stderr s
                    exitFailure
     Executed v t env -> do
                    putStrLn $ "- : " ++ (Type.typeToStr [] t) ++ " = " ++ (valueToStr v)
                    exitSuccess

runBlock = runBlockI

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
--  (v, True, False) -> (prettifyContents v) >>= putStrLn
  (v, False, False) -> execContents v
--  (v, _, True) -> (generateHSFromContents v) >>= putStrLn
mainEntry _ = return ()