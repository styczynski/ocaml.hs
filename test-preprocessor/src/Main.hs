module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Preprocessor

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= runBlock v

runBlockI :: Verbosity -> String -> IO ()
runBlockI v s = do
  putStrLn "Hello man!"

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