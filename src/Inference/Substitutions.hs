{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Inference.Substitutions where

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Type

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s (TDep name deps) = TDep name $ map (\a -> apply s a) deps
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply s (t1 `TTuple` t2) = apply s t1 `TTuple` apply s t2
  apply s (TList a) = TList $ apply s a
  apply s TUnit = TUnit
  apply s (TExport v) = (TExport v)

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TList a)      = ftv a
  ftv (TDep name deps) = foldl (\acc el -> acc `Set.union` (ftv el)) (Set.empty) deps
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TTuple` t2) = ftv t1 `Set.union` ftv t2
  ftv TUnit = Set.empty
  ftv (TExport _) = Set.empty

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable AConstraint where
   apply s (AConstraint l (t1, t2)) = AConstraint l (apply s t1, apply s t2)
   ftv (AConstraint _ (t1, t2)) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

(+>) :: Subst -> Subst -> Subst
(+>) (Subst s1) (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1
