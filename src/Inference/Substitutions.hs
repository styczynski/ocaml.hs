{-|
Module      : Inference.Substitution
Description : Substitution utilities
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides basic classes and their instances to manage
  free variable subsitution, binding types togehter and managing type evnironment variables.
-}
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

-- | Represents a type that can bind one type to the other one
class Bindable a b where
  (<->)     :: a -> b -> Either TypeError Subst

-- | Represents type that have entities that can be replaced
class Substitutable a where
  (.>)     :: Subst -> a -> a
  free   :: a -> Set.Set TypeVar
  isRecursive ::  TypeVar -> a -> Bool
  isRecursive a t = Set.member a $ free t

-- | This is used to set type variables during construction of type contraints
instance Bindable TypeVar Type where
  (<->) a t | t == (TypeVar a) = Right emptySubst
            | isRecursive a t  = Left $ InfiniteType EmptyPayload a t
            | otherwise        = Right (Subst $ Map.singleton a t)

-- | This is used to replace type variables within types
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

-- | This is used to replace type variables within types
instance Substitutable Scheme where
  (.>) (Subst s) (Scheme vars t) =
    Scheme vars $ (Subst $ foldr Map.delete s vars) .> t
  free (Scheme vars t) = free t `Set.difference` Set.fromList vars

-- | This is used to replace type variables within types
instance Substitutable TypeConstraint where
  (.>) s (TypeConstraint _ (t1, t2)) =
    TypeConstraint EmptyPayload (s .> t1, s .> t2)
  free (TypeConstraint _ (t1, t2)) = free t1 `Set.union` free t2

-- | This is used to replace type variables within types
instance (Substitutable a) => Substitutable [a] where
  (.>) s = map (s .>)
  free s = foldr (\el acc -> (free el) `Set.union` acc) Set.empty s

-- | This is used to replace entities withing typign environment
instance Substitutable TypeEnvironment where
  (.>) s (TypeEnvironment env) = TypeEnvironment $ Map.map (s .>) env
  free (TypeEnvironment env) = free $ Map.elems env

(+>) :: Subst -> Subst -> Subst
(+>) (Subst s1) (Subst s2) = Subst $ Map.map ((Subst s1) .>) s2 `Map.union` s1
