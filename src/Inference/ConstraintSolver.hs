{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.ConstraintSolver where

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Substitutions
import Inference.Errors
import Inference.Types

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set

type Solve = StateT (SolveState) (ExceptT TypeError IO)
data SolveState = SolveState { lastAnnot :: TypeErrorPayload }

initSolve :: SolveState
initSolve = SolveState { lastAnnot = EmptyPayload }

class BindableSolve a b where
  (<-$->) :: a -> b -> Solve Subst

instance BindableSolve TypeVar Type where
  (<-$->) a t = do
    payl <- errSolvePayload
    case a <-> t of
      Left (InfiniteType _ a t) -> throwError $ InfiniteType payl a t
      (Left v) -> throwError v
      (Right r) -> return r

instance BindableSolve Type Type where
  (<-$->) t1 t2 | t1 == t2 = return emptySubst
  (<-$->) (TypeVar v) t = v <-$-> t
  (<-$->) t (TypeVar v) = v <-$-> t
  (<-$->) t1@(TypeComplex name1 deps1) t2@(TypeComplex name2 deps2) = do
    payl <- errSolvePayload
    _ <- if not (name1 == name2) then throwError $ UnificationMismatch payl [t1] [t2] else return 0
    deps1 <-$-> deps2
  (<-$->) (TypeList t1) (TypeList t2) = [t1] <-$-> [t2]
  (<-$->) (TypeTuple t1 t2) (TypeTuple t3 t4) = [t1, t2] <-$-> [t3, t4]
  (<-$->) (TypeArrow t1 t2) (TypeArrow t3 t4) = [t1, t2] <-$-> [t3, t4]
  (<-$->) t1 t2 = do
    payl <- errSolvePayload
    throwError $ UnificationFail payl t1 t2

instance BindableSolve [Type] [Type] where
  (<-$->) [] [] = return emptySubst
  (<-$->) (t1 : ts1) (t2 : ts2) =
    do su1 <- t1 <-$-> t2
       su2 <- (su1 .> ts1) <-$-> (su1 .> ts2)
       return (su2 +> su1)
  (<-$->) t1 t2 = do
    payl <- errSolvePayload
    throwError $ UnificationMismatch payl t1 t2

checkpointAnnotSolve :: TypeConstraint -> Solve ()
checkpointAnnotSolve (TypeConstraint l _) = do
    s <- get
    put s{lastAnnot = l}
    return ()

errSolvePayload :: Solve TypeErrorPayload
errSolvePayload = do
  s <- get
  lastAnnot <- return $ let SolveState { lastAnnot = lastAnnot } = s in lastAnnot
  return lastAnnot

runSolve :: [TypeConstraint] -> IO (Either TypeError Subst)
runSolve cs = do
  r <- runExceptT (runStateT (solver (emptySubst, cs)) (initSolve))
  case r of
    Left e -> return $ Left e
    Right (s, _) -> return $ Right s

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((TypeConstraint l (t1, t2)): cs0) -> do
      checkpointAnnotSolve (TypeConstraint l (t1, t2))
      su1  <- t1 <-$-> t2
      solver (su1 +> su, su1 .> cs0)


