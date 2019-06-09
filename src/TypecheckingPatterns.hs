module TypecheckingPatterns where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import AbsSyntax
import Runtime
import Environment
import TypecheckingUtils

import qualified Data.Map as Map
import qualified Data.Set as Set

tiPattern :: TypeEnv -> SimplePattern -> (Subst, RuntimeType) -> TI (Subst, TypeEnv)
tiPattern env (PatIdent (Ident name)) (s1, t1) = do
  (TypeEnv env2) <- return $ remove env name
  t2 <- return $ generalize (apply s1 env) t1
  env3 <- return $ TypeEnv (Map.insert name t2 env2)
  return (s1, (apply s1 env3))

tiPattern env PatNone _ = return (nullSubst, TypeEnv $ Map.empty)

tiPattern env (PatList (PList elems)) val@(s1, t1) = do
  (case t1 of
    (TList innerType) -> do
      foldM (\(sAcc, TypeEnv eAcc) (PListElement pat) -> do
        (sPat, TypeEnv ePat) <- tiPattern env pat (s1, innerType)
        return ((sAcc `composeSubst` sPat), (TypeEnv $ eAcc `Map.union` ePat))) (nullSubst, TypeEnv $ Map.empty) elems
    _ -> throwError $ "Incompatible types for [] destructing pattern")



tiPattern env (PatCons ph pt) val@(s1, t1) = do
  (case t1 of
    (TList innerType) -> do
      (s1, TypeEnv e1) <- tiPattern env ph (s1, innerType)
      (s2, TypeEnv e2) <- tiPattern env pt val
      return ((s1 `composeSubst` s2), (TypeEnv (e1 `Map.union` e2)))
    _ -> throwError $ "Incompotible types (::)")