{-|
Module      : Interpreter.Definitions
Description : Code to handle runtime definitions
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module handles definitions of types in runtime.
-}
module Interpreter.Definitions where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.Map                      as Map

import           Runtime.Runtime
import           Runtime.Environment

import           Syntax.Base

createVariant :: Ident -> Environment -> TDefVariant -> Exec Environment
createVariant name env (TDefVarSimpl option) = do
  env1 <- return $ setVariable option (RVariant name option $ REmpty) env
  env2 <- return $ setDef option (DVariant name option) env1
  return env2
createVariant name env (TDefVarCompl option _) = do
  fnBody <- return $ \[arg] -> do
    env <- ask
    return ((RVariant name option $ arg), env)
  env1 <- return $ createFunction option Nothing (RFunSig 1) fnBody env
  env2 <- return $ setDef option (DVariant name option) env1
  return env2

constructVariantType
  :: Ident -> [TDefVariant] -> Exec (RuntimeValue, Environment)
constructVariantType name variants = do
  defEnv <- ask
  newEnv <- foldM (createVariant name) (defEnv) variants
  return (REmpty, newEnv)

createRecordField :: Ident -> Environment -> TDefRecord -> Exec Environment
createRecordField name env (TDefRecord fieldName fieldType) = do
  env1 <- return $ setDef fieldName (DRecord name fieldName) env
  return env1

constructRecordType :: Ident -> [TDefRecord] -> Exec (RuntimeValue, Environment)
constructRecordType name fields = do
  defEnv <- ask
  newEnv <- foldM (createRecordField name) (defEnv) fields
  return (REmpty, newEnv)

execTypeDef :: TypeDef -> Exec (RuntimeValue, Environment)
execTypeDef (TypeDefVar    _ name variants) = constructVariantType name variants
execTypeDef (TypeDefVarP   _ name variants) = constructVariantType name variants
execTypeDef (TypeDefRecord _ name fields  ) = constructRecordType name fields
