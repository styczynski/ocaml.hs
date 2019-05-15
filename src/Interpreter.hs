module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

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

