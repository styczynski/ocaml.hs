module Lib where

run :: Verbosity -> String -> IO ()
run v s = let ts = myLLexer s in case pImplementation ts of
           Ok  tree -> do
                          res <- runAST tree emptyEnv