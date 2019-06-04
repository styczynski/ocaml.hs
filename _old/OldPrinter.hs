module Printer where

import Control.Monad (when)

import AbsSyntax
import PrintSyntax

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

treeToStr :: (Show a, Print a) => a -> String
treeToStr tree = printTree tree
