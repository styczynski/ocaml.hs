module InterpreterDefinitions where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import Runtime
import Environment
import AbsSyntax

createVariant :: Ident -> Environment -> TDefVariant -> Exec Environment
createVariant name env (TDefVarSimpl option) = do
  return $ setVariable option (RVariant name option $ REmpty) env
createVariant name env (TDefVarCompl option _ _) = do
  fnBody <- return $ \[arg] -> do
    env <- ask
    return ((RVariant name option $ arg), env)
  return $ createFunction option (RFunSig 1) fnBody env

constructVariantType :: Ident -> [TDefVariant] -> Exec (RuntimeValue, Environment)
constructVariantType name variants = do
  defEnv <- ask
  newEnv <- foldM (createVariant name) (defEnv) variants
  return (REmpty, newEnv)

execTypeDef :: TypeDef -> Exec (RuntimeValue, Environment)
execTypeDef (TypeDefVar name variants) = constructVariantType name variants
execTypeDef (TypeDefVarP name variants) = constructVariantType name variants