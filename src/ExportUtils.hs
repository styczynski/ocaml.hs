{-# LANGUAGE FlexibleInstances #-}

module ExportUtils where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Exception
import System.IO.Unsafe
import Data.Typeable

import Runtime
import Environment

instance (UnpackableValue a, PackableValue b) => PackableValue (a -> b) where
  packVal innerFn = do
    env <- ask
    body <- return $ \[arg0] ->
        do { bodyEnv <- ask
           ; (val1, env1) <- unpackVal arg0
           ; r <- return $ innerFn val1
           ; local (\_ -> env1) $ packVal r }
    return $ newFunction (RFunSig 1) body env

instance (PackableValue a) => PackableValue (IO a) where
  packVal valIO = do
    r <- return $ unsafePerformIO $ try valIO
    case r of
      (Left (SomeException e)) -> raise $ "Error caused by inner IO exception" ++ (show (typeOf e))
      (Right v) -> packVal v