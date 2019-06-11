{-# LANGUAGE FlexibleInstances #-}

module ExportUtils where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Exception
import System.IO.Unsafe
import Data.Typeable
import qualified Data.Map as Map

import Runtime
import Environment

--instance (PackableValue a) => PackableValue [a] where
--  packVal vals = do
--    env <- ask
--    return ((RList $ mapM packVal vals), env)
--
--instance (UnpackableValue a) => UnpackableValue [a] where
--  unpackVal (RList vals) = do
--    env <- ask
--    (mapM unpackVal vals) >>= (\x -> return $ foldl (\(vals,oldEnv) (val,newEnv) -> (([val] ++ vals),(newEnv))) ([], env) x)

instance {-# OVERLAPS #-} (UnpackableValue a, UnpackableValue b, UnpackableValue c, UnpackableValue d, PackableValue e) => PackableValue (a -> b -> c -> d -> e) where
  packVal innerFn = do
    env <- ask
    body <- return $ \[arg0, arg1, arg2, arg3] ->
        do { bodyEnv <- ask
           ; (val1, env1) <- unpackVal arg0
           ; (val2, env2) <- local (\_ -> env1) $ unpackVal arg1
           ; (val3, env3) <- local (\_ -> env2) $ unpackVal arg2
           ; (val4, env4) <- local (\_ -> env3) $ unpackVal arg3
           ; r <- return $ innerFn val1 val2 val3 val4
           ; shadow bodyEnv $ local (\_ -> env4) $ packVal r }
    shadow env $ return $ newFunction (RFunSig 4) body env

instance {-# OVERLAPS #-} (UnpackableValue a, UnpackableValue b, UnpackableValue c, PackableValue d) => PackableValue (a -> b -> c -> d) where
  packVal innerFn = do
    env <- ask
    body <- return $ \[arg0, arg1, arg2] ->
        do { bodyEnv <- ask
           ; (val1, env1) <- unpackVal arg0
           ; (val2, env2) <- local (\_ -> env1) $ unpackVal arg1
           ; (val3, env3) <- local (\_ -> env2) $ unpackVal arg2
           ; r <- return $ innerFn val1 val2 val3
           ; shadow bodyEnv $ local (\_ -> env3) $ packVal r }
    shadow env $ return $ newFunction (RFunSig 3) body env

instance {-# OVERLAPS #-} (UnpackableValue a, UnpackableValue b, PackableValue c) => PackableValue (a -> b -> c) where
  packVal innerFn = do
    env <- ask
    body <- return $ \[arg0, arg1] ->
        do { bodyEnv <- ask
           ; (val1, env1) <- unpackVal arg0
           ; (val2, env2) <- local (\_ -> env1) $ unpackVal arg1
           ; r <- return $ innerFn val1 val2
           ; shadow bodyEnv $ local (\_ -> env2) $ packVal r }
    shadow env $ return $ newFunction (RFunSig 2) body env

instance {-# OVERLAPS #-} (UnpackableValue a, PackableValue b) => PackableValue (a -> b) where
  packVal innerFn = do
    env <- ask
    body <- return $ \[arg0] ->
        do { bodyEnv <- ask
           ; (val1, env1) <- unpackVal arg0
           ; r <- return $ innerFn val1
           ; shadow bodyEnv $ local (\_ -> env1) $ packVal r }
    shadow env $ return $ newFunction (RFunSig 1) body env

instance {-# OVERLAPS #-} (PackableValue a) => PackableValue (IO a) where
  packVal valIO = do
    r <- return $ unsafePerformIO $ try valIO
    case r of
      (Left (SomeException e)) -> raise $ show e
      (Right v) -> packVal v

instance {-# OVERLAPS #-} (PackableValue a) => PackableValue (Exec a) where
  packVal valExec = do
    valExec >>= packVal

instance PackableValue () where
  packVal _ = do
    env <- ask
    return (REmpty, env)

instance PackableValue (RuntimeValue, Environment) where
  packVal val = return val

instance PackableValue RuntimeValue where
  packVal val = do
    env <- ask
    return (val, env)

instance UnpackableValue RuntimeValue where
  unpackVal val = do
    env <- ask
    return (val, env)

instance UnpackableValue (RuntimeValue, Environment) where
  unpackVal val = do
    env <- ask
    return ((val, env), env)
