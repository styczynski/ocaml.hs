module Interpreter where

import Printer
import AbsSyntax

executeStatement :: ImplPhrase -> IO ()
executeStatement st = putStrLn ("\nExecute statement: " ++ (treeToStr st))

interpretAST :: Implementation -> IO ()
interpretAST tree = do
  case tree of
    IRootComplex st1 st2 -> do
      interpretAST st1
      interpretAST st2
    IRoot st -> do
      executeStatement st

