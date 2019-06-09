module TypecheckingUtils where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime
import Environment

import qualified Data.Map as Map
import qualified Data.Set as Set

type Subst = Map.Map String RuntimeType
newtype TypeEnv = TypeEnv (Map.Map String Scheme)

data Scheme = Scheme [String] RuntimeType

class Types a where
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a

instance Types RuntimeType where
  ftv TEmpty = Set.empty
  ftv (TList t1) = ftv t1
  ftv (TVar name) = Set.singleton name
  ftv TInt = Set.empty
  ftv (TFunEx t1 t2) = (ftv t1) `Set.union` (ftv t2)
  apply s (TVar name) = case Map.lookup name s of
    Nothing -> TVar name
    Just t -> t
  apply s (TList t1) = TList (apply s t1)
  apply s (TFunEx t1 t2) = TFunEx (apply s t1) (apply s t2)
  apply s t = t

instance Types Scheme where
  ftv (Scheme vars t) = (ftv t) Set.\\ (Set.fromList vars)
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance (Types a) => Types [a] where
  apply s = map (apply s)
  ftv l = foldr Set.union Set.empty (map ftv l)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> RuntimeType -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList ((ftv t) Set.\\ (ftv env))

data TIEnv = TIEnv { }
data TIState = TIState{
    tiSupply :: Int,
    tiSubst :: Subst
}

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a
runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res, st)
  where
    initTIEnv = TIEnv { }
    initTIState = TIState {
      tiSupply = 0,
      tiSubst = Map.empty }

newTyVar :: String -> TI RuntimeType
newTyVar prefix = do
  s <- get
  put s{ tiSupply = tiSupply s + 1 }
  return (TVar (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI RuntimeType
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTyVar "a") vars
  let s = Map.fromList (zip vars nvars) in
    return $ apply s t

mgu :: RuntimeType -> RuntimeType -> TI Subst
mgu (TList t1) (TList t2) = do
  s1 <- mgu t1 t2
  return s1
mgu (TFunEx l1 r1) (TFunEx l2 r2) = do
  s1 <- mgu l1 l2
  s2 <- mgu (apply s1 r1) (apply s1 r2)
  return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
varBind :: String -> RuntimeType -> TI Subst
varBind u t | t <- (TVar u) = return nullSubst
            | u `Set.member` ftv t = throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
            | otherwise = return (Map.singleton u t)

