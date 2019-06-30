{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Inference.Substitutions where

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Types

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set

class Substitutable a where
  (.>)     :: Subst -> a -> a
  free   :: a -> Set.Set TypeVar

instance Substitutable Type where
  (.>) _ (TypeStatic a)       = TypeStatic a
  (.>) s (TDep name deps) = TDep name $ map (\a -> s .> a) deps
  (.>) (Subst s) t@(TypeVar a) = Map.findWithDefault t a s
  (.>) s (t1 `TypeArrow` t2) = (s .> t1) `TypeArrow` (s .> t2)
  (.>) s (t1 `TypeTuple` t2) = (s .> t1) `TypeTuple` (s .> t2)
  (.>) s (TypeList a) = TypeList $ s .> a
  (.>) s TypeUnit = TypeUnit
  (.>) s (TExport v) = (TExport v)

  free TypeStatic{}         = Set.empty
  free (TypeVar a)       = Set.singleton a
  free (TypeList a)      = free a
  free (TDep name deps) = foldl (\acc el -> acc `Set.union` (free el)) (Set.empty) deps
  free (t1 `TypeArrow` t2) = free t1 `Set.union` free t2
  free (t1 `TypeTuple` t2) = free t1 `Set.union` free t2
  free TypeUnit = Set.empty
  free (TExport _) = Set.empty

instance Substitutable Scheme where
  (.>) (Subst s) (Forall as t)   = Forall as $ s' .> t
                            where s' = Subst $ foldr Map.delete s as
  free (Forall as t) = free t `Set.difference` Set.fromList as

instance Substitutable TypeConstraint where
   (.>) s (TypeConstraint _ (t1, t2)) = TypeConstraint EmptyPayload (s .> t1, s .> t2)
   free (TypeConstraint _ (t1, t2)) = free t1 `Set.union` free t2

instance Substitutable a => Substitutable [a] where
  (.>) s = map (s .>)
  free   = foldr (Set.union . free) Set.empty

instance Substitutable Env where
  (.>)  s (TypeEnv env) = TypeEnv $ Map.map (s .>) env
  free (TypeEnv env) = free $ Map.elems env

(+>) :: Subst -> Subst -> Subst
(+>) (Subst s1) (Subst s2) = Subst $ Map.map ((Subst s1) .>) s2 `Map.union` s1
