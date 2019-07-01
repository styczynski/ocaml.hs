{-|
Module      : Inference.TypingEnvironment
Description : All base definitions for inference
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides all base data types used by inference modules i.e.
  substitution mappings, typing environment, base typechecking errors etc.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.TypingEnvironment where

import           Inference.Types

import           Syntax.Base             hiding ( TypeConstraint )

import           Prelude                 hiding ( lookup )

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable           hiding ( toList )
import qualified Data.Map                      as Map

-- | Subsitution mapping
newtype Subst = Subst (Map.Map TypeVar Type) deriving (Eq, Show)

-- | Empty subsitution map that subsitutes nothing
emptySubst :: Subst
emptySubst = Subst $ Map.empty

-- | Payload for typechecking errors
data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)

-- | Type constraints with error payload annotation and unifier for types
data TypeConstraint = TypeConstraint TypeErrorPayload (Type, Type) deriving (Show)
type Unifier = (Subst, [TypeConstraint])

-- | Base inference monad and state
type Infer = StateT (InferState) (ReaderT (Env) (ExceptT TypeError IO))
data InferState = InferState { count :: Int, inferTrace :: [String], lastInferExpr :: String }

-- | Base typechecking errors
data TypeError
  = UnificationFail TypeErrorPayload Type Type
  | InfiniteType TypeErrorPayload TypeVar Type
  | UnboundVariable TypeErrorPayload Ident
  | Ambigious TypeErrorPayload [TypeConstraint]
  | UnificationMismatch TypeErrorPayload [Type] [Type]
  | Debug TypeErrorPayload String
  deriving (Show)

-- | Empty inference state
initInfer :: InferState
initInfer = InferState { count = 0, inferTrace = [], lastInferExpr = "" }

-- | Stringify type contraint (for debug purposes)
constraintToStr :: TypeConstraint -> String
constraintToStr (TypeConstraint _ (a, b)) =
  (typeToStr [] a) ++ " ~ " ++ (typeToStr [] b)

-- | Stringify type contraints list (for debug purposes)
constraintsListToStr :: [TypeConstraint] -> String
constraintsListToStr l =
  "{"
    ++ (foldr
         (\t acc ->
           acc
             ++ (if (length acc) <= 0 then "" else ", ")
             ++ (constraintToStr t)
         )
         ""
         l
       )
    ++ "}"

-- | Empty typing environment
empty :: Env
empty = TypeEnv Map.empty

-- | Create identificator inside the environment
(++>) :: Env -> (Ident, Scheme) -> Env
(++>) env (name, scheme) =
  let newEnv = env { types = Map.insert name scheme (types env) } in newEnv

-- | Remvoe identificator from the environment
(-->) :: Env -> Ident -> Env
(-->) (TypeEnv env) name = TypeEnv (Map.delete name env)

-- | Execute monad in shadowed environment
(==>) :: (Ident, Scheme) -> Infer a -> Infer a
(==>) (x, sc) m = local (\env -> (env --> x) ++> (x, sc)) m

-- | Lookup typing environment for the identificator
(??) :: Env -> Ident -> Maybe Scheme
(??) (TypeEnv env) name = Map.lookup name env
