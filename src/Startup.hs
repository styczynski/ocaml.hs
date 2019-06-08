module Startup where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime
import Environment
import ExportUtils
import Arithmetics

failwith :: String -> IO ()
failwith str = error $ "Failed with: " ++ str

invalid_arg :: String -> IO ()
invalid_arg str = error $ "Invalid_argument: " ++ str


interpreterStartupFn :: Exec (RuntimeValue, Environment)
interpreterStartupFn = do
  e <- ask
  (_,e) <- local (\_ -> e) $ setNativeVariable "failwith" failwith
  (_,e) <- local (\_ -> e) $ setNativeVariable "invalid_arg" invalid_arg
  --valueEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "valueEq" valueEq
  return (REmpty, e)