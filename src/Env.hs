{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env (
  Env(..),
  empty,
  lookup,
  remove,
  extend,
  extends,
  merge,
  mergeEnvs,
  singleton,
  keys,
  fromList,
  toList,
) where

import Prelude hiding (lookup)
import AbsSyntax
import Type

import Data.Monoid
import Data.Semigroup
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------


empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Ident, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> Ident -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Ident, Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Ident -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Ident -> Scheme -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [Ident]
keys (TypeEnv env) = Map.keys env

fromList :: [(Ident, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Ident, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Semigroup Env where
  (<>) = merge

instance Monoid Env where
  mempty = empty
  mappend = merge
