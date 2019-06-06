module Arithmetics where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime

valueEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueEq (RInt a) (RInt b) = return $ a == b
valueEq (RString a) (RString b) = return $ a == b
valueEq (RBool a) (RBool b) = return $ a == b
valueEq (RRef a) (RRef b) = return $ a == b
valueEq REmpty REmpty = return True
valueEq x y = do
  env <- ask
  raise $ "Could not compare (==) objects of type " ++ (getTypeStr (x, env)) ++ " and " ++ (getTypeStr (y, env))

valueLt :: RuntimeValue -> RuntimeValue -> Exec Bool
valueLt (RInt a) (RInt b) = return $ a < b
valueLt x y = do
  env <- ask
  raise $ "Could not compare (<) objects of type " ++ (getTypeStr (x, env)) ++ " and " ++ (getTypeStr (y, env))

valueGt :: RuntimeValue -> RuntimeValue -> Exec Bool
valueGt (RInt a) (RInt b) = return $ a > b
valueGt x y = do
  env <- ask
  raise $ "Could not compare (>) objects of type " ++ (getTypeStr (x, env)) ++ " and " ++ (getTypeStr (y, env))

valueLtEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueLtEq (RInt a) (RInt b) = return $ a <= b
valueLtEq x y = do
  env <- ask
  raise $ "Could not compare (<=) objects of type " ++ (getTypeStr (x, env)) ++ " and " ++ (getTypeStr (y, env))

valueGtEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueGtEq (RInt a) (RInt b) = return $ a >= b
valueGtEq x y = do
  env <- ask
  raise $ "Could not compare (>=) objects of type " ++ (getTypeStr (x, env)) ++ " and " ++ (getTypeStr (y, env))

valueCons :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueCons a (RList b) = return $ RList $ (a:b)
valueCons x y = do
  env <- ask
  raise $ "Could not append (::) element to list: " ++ (getTypeStr (x, env)) ++ " :: " ++ (getTypeStr (y, env))
