{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.TypingEnvironment where

import Inference.Type

import Prelude hiding (lookup)
import AbsSyntax

import Data.Monoid
import Data.Semigroup
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Show, Monoid, Semigroup)

data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)

type Constraint = (Type, Type)
data AConstraint = AConstraint TypeErrorPayload Constraint
type Unifier = (Subst, [AConstraint])

constraintToStr :: Constraint -> String
constraintToStr (a,b) = (typeToStr [] a) ++ " ~ " ++ (typeToStr [] b)

constraintsListToStr :: [Constraint] -> String
constraintsListToStr l = "{" ++ (foldr (\t acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ (constraintToStr t)) "" l) ++ "}"


empty :: Env
empty = TypeEnv Map.empty

(++>) :: Env -> (Ident, Scheme) -> Env
(++>) env (x, s) = env { types = Map.insert x s (types env) }

(-->) :: Env -> Ident -> Env
(-->) (TypeEnv env) var = TypeEnv (Map.delete var env)

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
