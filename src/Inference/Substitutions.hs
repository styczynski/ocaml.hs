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
  (.>)     :: Subst -> a -> a
  free   :: a -> Set.Set TVar

instance Substitutable Type where
  (.>) _ (TCon a)       = TCon a
  (.>) s (TDep name deps) = TDep name $ map (\a -> s .> a) deps
  (.>) (Subst s) t@(TVar a) = Map.findWithDefault t a s
  (.>) s (t1 `TArr` t2) = (s .> t1) `TArr` (s .> t2)
  (.>) s (t1 `TTuple` t2) = (s .> t1) `TTuple` (s .> t2)
  (.>) s (TList a) = TList $ s .> a
  (.>) s TUnit = TUnit
  (.>) s (TExport v) = (TExport v)

  free TCon{}         = Set.empty
  free (TVar a)       = Set.singleton a
  free (TList a)      = free a
  free (TDep name deps) = foldl (\acc el -> acc `Set.union` (free el)) (Set.empty) deps
  free (t1 `TArr` t2) = free t1 `Set.union` free t2
  free (t1 `TTuple` t2) = free t1 `Set.union` free t2
  free TUnit = Set.empty
  free (TExport _) = Set.empty

instance Substitutable Scheme where
  (.>) (Subst s) (Forall as t)   = Forall as $ s' .> t
                            where s' = Subst $ foldr Map.delete s as
  free (Forall as t) = free t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   (.>) s (t1, t2) = (s .> t1, s .> t2)
   free (t1, t2) = free t1 `Set.union` free t2

instance Substitutable AConstraint where
   (.>) s (AConstraint l (t1, t2)) = AConstraint l (s .> t1, s .> t2)
   free (AConstraint _ (t1, t2)) = free t1 `Set.union` free t2

instance Substitutable a => Substitutable [a] where
  (.>) s = map (s .>)
  free   = foldr (Set.union . free) Set.empty

instance Substitutable Env where
  (.>)  s (TypeEnv env) = TypeEnv $ Map.map (s .>) env
  free (TypeEnv env) = free $ Map.elems env

(+>) :: Subst -> Subst -> Subst
(+>) (Subst s1) (Subst s2) = Subst $ Map.map ((Subst s1) .>) s2 `Map.union` s1
