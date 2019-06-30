module Main where

import Runtime.Runtime

import Options.Applicative
import Data.Semigroup ((<>))

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Lib
import Interpreter.Definitions

import qualified Inference.Type as Type
import Inference.Errors
import Inference.Inferencer

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
                    hPutStrLn stderr $ typeErrorToStr  s
                    exitFailure
     FailedExecution s -> do
                    hPutStrLn stderr s
                    exitFailure
     FailedParse s  -> do
                    hPutStrLn stderr s
                    exitFailure
     Executed v t env -> do
                    putStrLn $ "- : " ++ (Type.typeToStr [] t) ++ " = " ++ (valueToStr env v)
                    exitSuccess

runBlock = runBlockI

execContents v = getContents >>= runBlock v

data MainArgs = MainArgs
  { verbosity :: Int
  , file :: String }

parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> strOption
      ( long "file"
      <> short 'f'
      <> showDefault
      <> value "stdin"
      <> help "File to load or stdin to load standard input"
      <> metavar "FILENAME" )

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Tiny Ocaml interprter for Haskell"
      <> header "Piotr Styczynski 2019" )

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity file) = case (verbosity, file) of
  (v, "stdin") -> execContents v
  (v, src) -> runFile v src
mainEntry _ = return ()