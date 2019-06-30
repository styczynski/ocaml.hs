{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.Substitutions where

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

class Bindable a b where
  (<->)     :: a -> b -> Either TypeError Subst

class Substitutable a where
  (.>)     :: Subst -> a -> a
  free   :: a -> Set.Set TypeVar
  isRecursive ::  TypeVar -> a -> Bool
  isRecursive a t = Set.member a $ free t

instance Bindable TypeVar Type where
  (<->) a t | t == (TypeVar a) = Right emptySubst
            | isRecursive a t  = Left $ InfiniteType EmptyPayload a t
            | otherwise        = Right (Subst $ Map.singleton a t)

instance Substitutable Type where
  (.>) _         (  TypeStatic a         ) = TypeStatic a
  (.>) s (TypeComplex name deps) = TypeComplex name $ map (\a -> s .> a) deps
  (.>) (Subst s) t@(TypeVar a            ) = Map.findWithDefault t a s
  (.>) s         (  t1 `TypeArrow` t2    ) = TypeArrow (s .> t1) (s .> t2)
  (.>) s         (  t1 `TypeTuple` t2    ) = TypeTuple (s .> t1) (s .> t2)
  (.>) s         (  TypeList a           ) = TypeList $ s .> a
  (.>) s         TypeUnit                  = TypeUnit
  (.>) s         (TypeAnnotated v)         = (TypeAnnotated v)

  free (TypeStatic a) = Set.empty
  free (TypeVar    a) = Set.singleton a
  free (TypeList   a) = free a
  free (TypeComplex name deps) =
    foldl (\acc el -> Set.union acc $ free el) (Set.empty) deps
  free (TypeArrow t1 t2) = Set.union (free t1) (free t2)
  free (TypeTuple t1 t2) = Set.union (free t1) (free t2)
  free TypeUnit          = Set.empty
  free (TypeAnnotated _) = Set.empty

instance Substitutable Scheme where
  (.>) (Subst s) (Forall vars t) =
    Forall vars $ (Subst $ foldr Map.delete s vars) .> t
  free (Forall vars t) = free t `Set.difference` Set.fromList vars

instance Substitutable TypeConstraint where
  (.>) s (TypeConstraint _ (t1, t2)) =
    TypeConstraint EmptyPayload (s .> t1, s .> t2)
  free (TypeConstraint _ (t1, t2)) = free t1 `Set.union` free t2

instance (Substitutable a) => Substitutable [a] where
  (.>) s = map (s .>)
  free s = foldr (\el acc -> (free el) `Set.union` acc) Set.empty s

instance Substitutable Env where
  (.>) s (TypeEnv env) = TypeEnv $ Map.map (s .>) env
  free (TypeEnv env) = free $ Map.elems env

(+>) :: Subst -> Subst -> Subst
(+>) (Subst s1) (Subst s2) = Subst $ Map.map ((Subst s1) .>) s2 `Map.union` s1
