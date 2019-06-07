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
  env1 <- return $ setVariable option (RVariant name option $ REmpty) env
  env2 <- return $ setDef option (DVariant name option) env1
  return env2
createVariant name env (TDefVarCompl option _) = do
  fnBody <- return $ \[arg] -> do
    env <- ask
    return ((RVariant name option $ arg), env)
  env1 <- return $ createFunction option (RFunSig 1) fnBody env
  env2 <- return $ setDef option (DVariant name option) env1
  return env2

constructVariantType :: Ident -> [TDefVariant] -> Exec (RuntimeValue, Environment)
constructVariantType name variants = do
  defEnv <- ask
  newEnv <- foldM (createVariant name) (defEnv) variants
  return (REmpty, newEnv)

execTypeDef :: TypeDef -> Exec (RuntimeValue, Environment)
execTypeDef (TypeDefVar name variants) = constructVariantType name variants
execTypeDef (TypeDefVarP name variants) = constructVariantType name variants