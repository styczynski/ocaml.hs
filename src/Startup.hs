module Startup where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime
import Environment
import ExportUtils

myfun :: Integer -> Integer
myfun a = a+7

myfunShow :: Integer -> String
myfunShow a = show a

interpreterStartupFn :: Exec (RuntimeValue, Environment)
interpreterStartupFn = do
  e <- ask
  (_,e) <- local (\_ -> e) $ setNativeVariable "myfun" myfun
  (_,e) <- local (\_ -> e) $ setNativeVariable "myfunShow" myfunShow
  return (REmpty, e)