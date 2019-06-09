module Arithmetics where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsSyntax
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

valueNot :: RuntimeValue -> Exec Bool
valueNot (RBool a) = return $ not a
valueNot x = do
  env <- ask
  raise $ "Could not negate (!) value: " ++ (getTypeStr (x, env))

valueOr :: RuntimeValue -> RuntimeValue ->Exec Bool
valueOr (RBool a) (RBool b) = return $ a || b
valueOr x y = do
  env <- ask
  raise $ "Could not calculate alternative (or): " ++ (getTypeStr (x, env)) ++ " || " ++ (getTypeStr (y, env))

valueAnd :: RuntimeValue -> RuntimeValue -> Exec Bool
valueAnd (RBool a) (RBool b) = return $ a && b
valueAnd x y = do
  env <- ask
  raise $ "Could not calculate logical and (and): " ++ (getTypeStr (x, env)) ++ " && " ++ (getTypeStr (y, env))

valueSub :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueSub (RInt a) (RInt b) = return $ RInt $ a - b
valueSub x y = do
  env <- ask
  raise $ "Could not substract values: " ++ (getTypeStr (x, env)) ++ " - " ++ (getTypeStr (y, env))

valueAdd :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueAdd (RInt a) (RInt b) = return $ RInt $ a + b
valueAdd x y = do
  env <- ask
  raise $ "Could not add values: " ++ (getTypeStr (x, env)) ++ " + " ++ (getTypeStr (y, env))

valueMul :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueMul (RInt a) (RInt b) = return $ RInt $ a * b
valueMul x y = do
  env <- ask
  raise $ "Could not multiply values: " ++ (getTypeStr (x, env)) ++ " * " ++ (getTypeStr (y, env))

valueDiv :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueDiv (RInt a) (RInt b) = return $ RInt $ a `div` b
valueDiv x y = do
  env <- ask
  raise $ "Could not divide values: " ++ (getTypeStr (x, env)) ++ " / " ++ (getTypeStr (y, env))

valueMod :: RuntimeValue -> RuntimeValue -> Exec Integer
valueMod (RInt a) (RInt b) = return $ mod a b
valueMod x y = do
  env <- ask
  raise $ "Could not calculate modulo (mod) of value: " ++ (getTypeStr (x, env)) ++ " % " ++ (getTypeStr (y, env))

valueJoin :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueJoin (RList a) (RList b) = return $ RList $ a ++ b
valueJoin x y = do
  env <- ask
  raise $ "Could not join (@) lists: " ++ (getTypeStr (x, env)) ++ " @ " ++ (getTypeStr (y, env))

valueSel :: RuntimeValue -> Ident -> Exec RuntimeValue
valueSel val@(RRecord _ fields) name = do
  env <- ask
  selVal <- return $ Map.findWithDefault RInvalid name fields
  case selVal of
    RInvalid -> raise $ "Could not get field " ++ (treeToStr name) ++ " of value of type " ++ (getTypeStr (val, env))
    val -> return $ val
valueSel val name = do
  env <- ask
  raise $ "Could not get field " ++ (treeToStr name) ++ " of value of type " ++ (getTypeStr (val, env))