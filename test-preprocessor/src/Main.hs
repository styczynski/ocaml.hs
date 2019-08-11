module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Preprocessor

data MainArgs = MainArgs
  { verbosity :: Int
  , inputDirectory :: String }

parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> strOption
      ( long "inputDirectory"
      <> short 'd'
      <> showDefault
      <> value "examples"
      <> help "Direcotry to look for tests input code"
      <> metavar "INPUT_DIRECTORY" )

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Tiny Ocaml interprter for Haskell"
      <> header "Piotr Styczynski 2019" )

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity file) = case (verbosity, file) of
  (v, dir) -> preprocessDirectory dir
mainEntry _ = return ()