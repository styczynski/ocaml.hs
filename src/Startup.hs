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

print_str :: RuntimeValue -> IO ()
print_str v = putStrLn $ valueToStr v

print_env :: RuntimeValue -> Exec String
print_env _ = do
  env <- ask
  runtimePrint $ "[Environment]: " ++ (envToStr env)
  return $ "Done."

interpreterStartupFn :: Exec (RuntimeValue, Environment)
interpreterStartupFn = do
  e <- ask
  (_,e) <- local (\_ -> e) $ setNativeVariable "export_env" valueExportEnv
  (_,e) <- local (\_ -> e) $ setNativeVariable "print" print_str
  (_,e) <- local (\_ -> e) $ setNativeVariable "print_env" print_env
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_create_ref" valueCreateRef
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_set_ref" valueSetRef
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_get_ref" valueGetRef
  (_,e) <- local (\_ -> e) $ setNativeVariable "failwith" failwith
  (_,e) <- local (\_ -> e) $ setNativeVariable "invalid_arg" invalid_arg
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_eq" valueEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_lt" valueLt
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_gt" valueGt
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_lt_eq" valueLtEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_gt_eq" valueGtEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_cons" valueCons
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_join" valueJoin
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_and" valueAnd
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_or" valueOr
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_not" valueNot
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_mod" valueMod
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_add" valueAdd
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_sub" valueSub
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_mul" valueMul
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_div" valueDiv
  return (REmpty, e)