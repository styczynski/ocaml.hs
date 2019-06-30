{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.TypingEnvironment where

import Inference.Type

import Syntax.Base

import Prelude hiding (lookup)

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable hiding (toList)
import qualified Data.Map as Map

newtype Subst = Subst (Map.Map TVar Type) deriving (Eq, Show)

data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)

type Constraint = (Type, Type)
data AConstraint = AConstraint TypeErrorPayload Constraint
type Unifier = (Subst, [AConstraint])

type Infer = StateT (InferState) (ReaderT (Env) (ExceptT TypeError IO))
data InferState = InferState { count :: Int, inferTrace :: [String], lastInferExpr :: String }

data TypeError
  = UnificationFail TypeErrorPayload Type Type
  | InfiniteType TypeErrorPayload TVar Type
  | UnboundVariable TypeErrorPayload Ident
  | Ambigious TypeErrorPayload [Constraint]
  | UnificationMismatch TypeErrorPayload [Type] [Type]
  | Debug TypeErrorPayload String
  deriving (Show)

initInfer :: InferState
initInfer = InferState { count = 0, inferTrace = [], lastInferExpr = "" }

constraintToStr :: Constraint -> String
constraintToStr (a,b) = (typeToStr [] a) ++ " ~ " ++ (typeToStr [] b)

constraintsListToStr :: [Constraint] -> String
constraintsListToStr l = "{" ++ (foldr (\t acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ (constraintToStr t)) "" l) ++ "}"

constraintDeannot :: AConstraint -> Constraint
constraintDeannot (AConstraint _ ac) = ac

constraintDeannotList :: [AConstraint] -> [Constraint]
constraintDeannotList acl = map constraintDeannot acl

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