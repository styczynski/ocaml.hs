module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Lib

data MainArgs = MainArgs
  { verbosity :: Int
  , prettify :: Bool }

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

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Tiny Ocaml interprter for Haskell"
      <> header "Piotr Styczynski 2019" )

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity prettify) = case (verbosity, prettify) of
  (v, True) -> (prettifyContents v) >>= putStrLn
  (v, False) -> execContents v
mainEntry _ = return ()