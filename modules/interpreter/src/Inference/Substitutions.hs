{-|
Module      : Inference.Substitution
Description : Substitution utilities
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides basic classes and their instances to manage
  freeDimensions variable subsitution, binding types togehter and managing type evnironment variables.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
  (<->)     :: a -> b -> Either TypeError (Substitution a b)

class (Ord b) => WithFreedom a b where
  freeDimensions   :: a -> Set.Set b
  freeDimensionsList :: a -> [b]
  freeDimensionsList a = Set.toList (freeDimensions a)
  isRecursive ::  b -> a -> Bool
  isRecursive a t = Set.member a $ freeDimensions t


class (Ord b, Monad m) => WithFreedomM a b m where
  freeDimensionsM   :: a -> m (Set.Set b)
  freeDimensionsListM :: a -> m [b]
  freeDimensionsListM a = do
    f <- freeDimensionsM a
    return $ Set.toList f
  isRecursiveM ::  b -> a -> m Bool
  isRecursiveM a t = do
    f <- freeDimensionsM t
    return $ Set.member a $ f

-- | Represents type that have entities that can be replaced
class (WithFreedom a b) => Substitutable a b c where
  (.>)     :: (Substitution b c) -> a -> a

-- | This is used to set type variables during construction of type contraints
instance Bindable TypeVar Type where
  (<->) a t | t == (TypeVar a) = Right emptySubst
            | isRecursive a t  = Left $ InfiniteType EmptyPayload a t
            | otherwise        = Right (Subst $ Map.singleton a t)

instance WithFreedom Type TypeVar where
  freeDimensions (TypeStatic a) = Set.empty
  freeDimensions (TypeVar    a) = Set.singleton a
  freeDimensions (TypeList   a) = freeDimensions a
  freeDimensions (TypeComplex name deps) =
    foldl (\acc el -> Set.union acc $ freeDimensions el) (Set.empty) deps
  freeDimensions (TypePoly alt) =
    foldl (\acc el -> Set.union acc $ freeDimensions el) (Set.empty) alt
  freeDimensions (TypeArrow t1 t2) =
    Set.union (freeDimensions t1) (freeDimensions t2)
  freeDimensions (TypeTuple t1 t2) =
    Set.union (freeDimensions t1) (freeDimensions t2)
  freeDimensions TypeUnit          = Set.empty
  freeDimensions (TypeAnnotated _) = Set.empty
  freeDimensions _                 = Set.empty

-- | This is used to replace type variables within types
instance Substitutable Type TypeVar Type where
  (.>) _         (  TypeStatic a         ) = TypeStatic a
  (.>) s (TypeComplex name deps) = TypeComplex name $ map (\a -> s .> a) deps
  (.>) (Subst s) t@(TypeVar a            ) = Map.findWithDefault t a s
  (.>) s         (  t1 `TypeArrow` t2    ) = TypeArrow (s .> t1) (s .> t2)
  (.>) s         (  t1 `TypeTuple` t2    ) = TypeTuple (s .> t1) (s .> t2)
  (.>) s         (  TypeList a           ) = TypeList $ s .> a
  (.>) s         TypeUnit                  = TypeUnit
  (.>) s         (TypeAnnotated v  )       = (TypeAnnotated v)
  (.>) s         (TypePoly      alt)       = TypePoly $ map (\a -> s .> a) alt

instance WithFreedom Scheme TypeVar where
  freeDimensions (Scheme vars t) =
    freeDimensions t `Set.difference` Set.fromList vars

-- | This is used to replace type variables within types
instance Substitutable Scheme TypeVar Type where
  (.>) (Subst s) (Scheme vars t) =
    Scheme vars $ (Subst $ foldr Map.delete s vars) .> t

instance WithFreedom TypeConstraint TypeVar where
  freeDimensions (TypeConstraint _ (t1, t2)) =
    freeDimensions t1 `Set.union` freeDimensions t2

-- | This is used to replace type variables within types
instance Substitutable TypeConstraint TypeVar Type where
  (.>) s (TypeConstraint _ (t1, t2)) =
    TypeConstraint EmptyPayload (s .> t1, s .> t2)

instance (WithFreedom a b) => WithFreedom [a] b where
  freeDimensions s =
    foldr (\el acc -> (freeDimensions el) `Set.union` acc) Set.empty s

-- | This is used to replace type variables within types
instance (Substitutable a b c) => Substitutable [a] b c where
  (.>) s = map (s .>)

instance WithFreedom TypeEnvironment TypeVar where
  freeDimensions (TypeEnvironment env) = freeDimensions $ Map.elems env

-- | This is used to replace entities withing typign environment
instance Substitutable TypeEnvironment TypeVar Type where
  (.>) s (TypeEnvironment env) = TypeEnvironment $ Map.map (s .>) env

class (WithFreedom a c, WithFreedom b c) => Generalizable a b c where
   generalized :: b -> a -> [c]

instance Generalizable Type TypeEnvironment TypeVar where
 generalized b a = Set.toList $ (freeDimensions a) `Set.difference` (freeDimensions b)

(+>)
  :: (Ord a, Substitutable b a b)
  => (Substitution a b)
  -> (Substitution a b)
  -> Substitution a b
(+>) (Subst s1) (Subst s2) = Subst $ Map.map ((Subst s1) .>) s2 `Map.union` s1
