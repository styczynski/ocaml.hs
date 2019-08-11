module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Preprocessor

data MainArgs = MainArgs
  { verbosity :: Int
  , inputDirectory :: String
  , outputDirectory :: String
  , testPrefix :: String }

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
      <> short 'i'
      <> showDefault
      <> value "examples"
      <> help "Direcotry to look for tests input code"
      <> metavar "INPUT_DIRECTORY" )
  <*> strOption
        ( long "outputDirectory"
        <> short 'o'
        <> showDefault
        <> value "test"
        <> help "Direcotry to look for tests input code"
        <> metavar "OUTPUT_DIRECTORY" )
  <*> strOption
          ( long "testPrefix"
          <> short 'p'
          <> showDefault
          <> value ""
          <> help "Direcotry to look for tests input code"
          <> metavar "TEST_PREFIX" )

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Tiny Ocaml interprter for Haskell"
      <> header "Piotr Styczynski 2019" )

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity file out prefix) = case (verbosity, file, out, prefix) of
  (v, dir, out, prefix) -> preprocessDirectory prefix out dir
mainEntry _ = return ()