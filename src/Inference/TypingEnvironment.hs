{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.TypingEnvironment where

import Inference.Types

import Syntax.Base hiding (TypeConstraint)

import Prelude hiding (lookup)

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

newtype Subst = Subst (Map.Map TypeVar Type) deriving (Eq, Show)

data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)

data TypeConstraint = TypeConstraint TypeErrorPayload (Type, Type) deriving (Show)
type Unifier = (Subst, [TypeConstraint])

type Infer = StateT (InferState) (ReaderT (Env) (ExceptT TypeError IO))
data InferState = InferState { count :: Int, inferTrace :: [String], lastInferExpr :: String }

data TypeError
  = UnificationFail TypeErrorPayload Type Type
  | InfiniteType TypeErrorPayload TypeVar Type
  | UnboundVariable TypeErrorPayload Ident
  | Ambigious TypeErrorPayload [TypeConstraint]
  | UnificationMismatch TypeErrorPayload [Type] [Type]
  | Debug TypeErrorPayload String
  deriving (Show)

initInfer :: InferState
initInfer = InferState { count = 0, inferTrace = [], lastInferExpr = "" }

constraintToStr :: TypeConstraint -> String
constraintToStr (TypeConstraint _ (a,b)) = (typeToStr [] a) ++ " ~ " ++ (typeToStr [] b)

constraintsListToStr :: [TypeConstraint] -> String
constraintsListToStr l = "{" ++ (foldr (\t acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ (constraintToStr t)) "" l) ++ "}"

empty :: Env
empty = TypeEnv Map.empty

(++>) :: Env -> (Ident, Scheme) -> Env
(++>) env (name, scheme) =
  let newEnv = env { types = Map.insert name scheme (types env) } in
  newEnv

(-->) :: Env -> Ident -> Env
(-->) (TypeEnv env) name = TypeEnv (Map.delete name env)

(==>) :: (Ident, Scheme) -> Infer a -> Infer a
(==>) (x, sc) m = local (\env -> (env --> x) ++> (x, sc)) m

(??) :: Env -> Ident -> Maybe Scheme
(??) (TypeEnv env) name = Map.lookup name env
