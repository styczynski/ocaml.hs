{-|
Module      : Interop.Arithmetics
Description : Base arithmetics module
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This code provides basic arithmetics for runtime values.
-}
module Interop.Arithmetics where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import           Data.List.Split
import           Data.Foldable

import           Syntax.Base

import           Runtime.Runtime
import           Runtime.Environment

valueStringifyRaw :: RuntimeValue -> String
valueStringifyRaw (RString v) = v
valueStringifyRaw v           = valueToStrN v

valueStringify :: RuntimeValue -> Exec String
valueStringify v = return $ valueStringifyRaw v

valueSplit :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueSplit (RString delim) (RString str) =
  return $ RList $ map (\e -> RString e) $ splitOn delim str
valueSplit _ _ = raise $ "Cannot perform split operation on the given types"

valueEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueEq (RInt    a) (RInt    b) = return $ a == b
valueEq (RString a) (RString b) = return $ a == b
valueEq (RBool   a) (RBool   b) = return $ a == b
valueEq (RRef    a) (RRef    b) = return $ a == b
valueEq (RTag name1 a) (RTag name2 b) = do
  innerEq <- valueEq a b
  return $ innerEq && (name1 == name2)
valueEq (RTuple  a) (RTuple  b) = do
  lenA        <- return $ length a
  lenB        <- return $ length b
  lenEqCond   <- return $ lenA == lenB
  innerEqCond <-
    foldrM
        (\(elA, elB) acc -> do
          t <- valueEq elA elB
          return $ t && acc
        )
        True
      $ zip a b
  return $ lenEqCond && innerEqCond
valueEq (RList a) (RList b) = do
  lenA        <- return $ length a
  lenB        <- return $ length b
  lenEqCond   <- return $ lenA == lenB
  innerEqCond <-
    foldrM
        (\(elA, elB) acc -> do
          t <- valueEq elA elB
          return $ t && acc
        )
        True
      $ zip a b
  return $ lenEqCond && innerEqCond
valueEq (RVariant a1 a2 t1) (RVariant b1 b2 t2) = do
  cmp <- valueEq t1 t2
  return $ (a1 == b1) && (a2 == b2) && cmp
valueEq REmpty REmpty = return True
valueEq _      _      = return False

valueLt :: RuntimeValue -> RuntimeValue -> Exec Bool
valueLt (RInt a) (RInt b) = return $ a < b
valueLt x        y        = do
  env <- ask
  raise
    $  "Could not compare (<) objects of type "
    ++ (getTypeStr (x, env))
    ++ " and "
    ++ (getTypeStr (y, env))

valueGt :: RuntimeValue -> RuntimeValue -> Exec Bool
valueGt (RInt a) (RInt b) = return $ a > b
valueGt x        y        = do
  env <- ask
  raise
    $  "Could not compare (>) objects of type "
    ++ (getTypeStr (x, env))
    ++ " and "
    ++ (getTypeStr (y, env))

valueLtEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueLtEq (RInt a) (RInt b) = return $ a <= b
valueLtEq x        y        = do
  env <- ask
  raise
    $  "Could not compare (<=) objects of type "
    ++ (getTypeStr (x, env))
    ++ " and "
    ++ (getTypeStr (y, env))

valueGtEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueGtEq (RInt a) (RInt b) = return $ a >= b
valueGtEq x        y        = do
  env <- ask
  raise
    $  "Could not compare (>=) objects of type "
    ++ (getTypeStr (x, env))
    ++ " and "
    ++ (getTypeStr (y, env))

valueCons :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueCons a (RList b) = return $ RList $ (a : b)
valueCons x y         = do
  env <- ask
  raise
    $  "Could not append (::) element to list: "
    ++ (getTypeStr (x, env))
    ++ " :: "
    ++ (getTypeStr (y, env))

valueNot :: RuntimeValue -> Exec RuntimeValue
valueNot (    RBool a) = return $ RBool $ not a
valueNot val@(RRef  _) = do
  (refVal, _) <- valueGetRef val
  return refVal
valueNot x = do
  env <- ask
  raise $ "Could not negate (!) value: " ++ (getTypeStr (x, env))

valueOr :: RuntimeValue -> RuntimeValue -> Exec Bool
valueOr (RBool a) (RBool b) = return $ a || b
valueOr x         y         = do
  env <- ask
  raise
    $  "Could not calculate alternative (or): "
    ++ (getTypeStr (x, env))
    ++ " || "
    ++ (getTypeStr (y, env))

valueAnd :: RuntimeValue -> RuntimeValue -> Exec Bool
valueAnd (RBool a) (RBool b) = return $ a && b
valueAnd x         y         = do
  env <- ask
  raise
    $  "Could not calculate logical and (and): "
    ++ (getTypeStr (x, env))
    ++ " && "
    ++ (getTypeStr (y, env))

valueSub :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueSub (RInt a) (RInt b) = return $ RInt $ a - b
valueSub x        y        = do
  env <- ask
  raise
    $  "Could not substract values: "
    ++ (getTypeStr (x, env))
    ++ " - "
    ++ (getTypeStr (y, env))

valueAdd :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueAdd (RInt a) (RInt b) = return $ RInt $ a + b
valueAdd x        y        = do
  env <- ask
  raise
    $  "Could not add values: "
    ++ (getTypeStr (x, env))
    ++ " + "
    ++ (getTypeStr (y, env))

valueMul :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueMul (RInt a) (RInt b) = return $ RInt $ a * b
valueMul x        y        = do
  env <- ask
  raise
    $  "Could not multiply values: "
    ++ (getTypeStr (x, env))
    ++ " * "
    ++ (getTypeStr (y, env))

valueDiv :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueDiv (RInt a) (RInt b) = do
  _ <- if b == 0
    then raise $ "Arithemtics error: Division by 0"
    else return REmpty
  return $ RInt $ a `div` b
valueDiv x y = do
  env <- ask
  raise
    $  "Could not divide values: "
    ++ (getTypeStr (x, env))
    ++ " / "
    ++ (getTypeStr (y, env))

valueMod :: RuntimeValue -> RuntimeValue -> Exec Integer
valueMod (RInt a) (RInt b) = do
  _ <- if b == 0
    then raise $ "Arithemtics error: Modulo of division by 0"
    else return REmpty
  return $ mod a b
valueMod x y = do
  env <- ask
  raise
    $  "Could not calculate modulo (mod) of value: "
    ++ (getTypeStr (x, env))
    ++ " % "
    ++ (getTypeStr (y, env))

valueJoin :: RuntimeValue -> RuntimeValue -> Exec RuntimeValue
valueJoin (RString a) (RString b) = return $ RString $ a ++ b
valueJoin (RList   a) (RList   b) = return $ RList $ a ++ b
valueJoin x           y           = do
  env <- ask
  raise
    $  "Could not join (@) lists: "
    ++ (getTypeStr (x, env))
    ++ " @ "
    ++ (getTypeStr (y, env))

valueSel :: RuntimeValue -> Ident -> Exec RuntimeValue
valueSel val@(RRecord _ fields) name = do
  env    <- ask
  selVal <- return $ Map.findWithDefault RInvalid name fields
  case selVal of
    RInvalid ->
      raise
        $  "Could not get field "
        ++ (treeToStr name)
        ++ " of value of type "
        ++ (getTypeStr (val, env))
    val -> return $ val
valueSel val name = do
  env <- ask
  raise
    $  "Could not get field "
    ++ (treeToStr name)
    ++ " of value of type "
    ++ (getTypeStr (val, env))

valueCreateRef :: RuntimeValue -> Exec (RuntimeValue, Environment)
valueCreateRef v = do
  env         <- ask
  (fr, frEnv) <- return $ allocRef env
  return ((RRef fr), (setRefStorage fr (RfVal v) frEnv))

valueGetRef :: RuntimeValue -> Exec (RuntimeValue, Environment)
valueGetRef p = do
  env <- ask
  case getRefStorage p env of
    (RfVal v) -> return (v, env)
    _ -> raise $ "Missing reffered object. There's probably bug in a code."

valueSetRef :: RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
valueSetRef (RRef fr) v = do
  env        <- ask
  env2       <- return $ setRefStorage fr (RfVal v) env
  (RfVal vv) <- return $ getRefStorage (RRef fr) env2
  return (vv, env2)

valueExportEnv :: RuntimeValue -> Exec (RuntimeValue, Environment)
valueExportEnv _ = do
  state <- get
  env   <- ask
  put $ state { globalExportEnv = (Just env) }
  return (REmpty, env)
