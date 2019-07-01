{-|
Module      : Inference.ContraintSolver
Description : Solver for  inference constraints
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This is code for contraint solver that tries
  to unify the types matching all provided contraints.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.ConstraintSolver where

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Substitutions
import           Inference.Errors
import           Inference.Types

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | Monad presenting solver computation
type Solve = StateT SolverState (ExceptT TypeError IO)

-- | Solver state
data SolverState = SolverState {
  lastAnnot :: TypeErrorPayload
}

-- | Empty solver state
emptySolverState :: SolverState
emptySolverState = SolverState { lastAnnot = EmptyPayload }

-- | Gets type cntraint and records its annotation inside solver state
--   This is for purely debug purposes
checkpointAnnotSolve :: TypeConstraint -> Solve ()
checkpointAnnotSolve (TypeConstraint l _) = do
  s <- get
  put s { lastAnnot = l }
  return ()

-- | Generate error payload from current solver state (for debug purposes)
errSolvePayload :: Solve TypeErrorPayload
errSolvePayload = do
  s         <- get
  lastAnnot <-
    return $ let SolverState { lastAnnot = lastAnnot } = s in lastAnnot
  return lastAnnot

-- | Runs solve monad for given contraints
runSolve :: [TypeConstraint] -> IO (Either TypeError Subst)
runSolve cs = do
  r <- runExceptT (runStateT (solver (emptySubst, cs)) emptySolverState)
  case r of
    Left  e      -> return $ Left e
    Right (s, _) -> return $ Right s

-- | Runs solver to unify all types
solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
  [] -> return su
  ((TypeConstraint l (t1, t2)):cs0) -> do
    checkpointAnnotSolve (TypeConstraint l (t1, t2))
    su1 <- t1 <-$-> t2
    solver (su1 +> su, su1 .> cs0)

-- | Represents a data type that can bind values of one type to the other one
class BindableSolve a b where
  (<-$->) :: a -> b -> Solve Subst

-- | This instance represents binding type variables to their types (creating contraints)
instance BindableSolve TypeVar Type where
  (<-$->) a t = do
    payl <- errSolvePayload
    case a <-> t of
      Left (InfiniteType _ a t) -> throwError $ InfiniteType payl a t
      (Left  v)                 -> throwError v
      (Right r)                 -> return r

-- | This instance represents binding type to type (unification)
instance BindableSolve Type Type where
  (<-$->) t1 t2 | t1 == t2 = return emptySubst
  (<-$->) (TypeVar v)                  t                            = v <-$-> t
  (<-$->) t                            (   TypeVar  v             ) = v <-$-> t
  (<-$->) (TypeList t1) (TypeList t2) = [t1] <-$-> [t2]
  (<-$->) (TypeTuple t1 t2) (TypeTuple t3 t4) = [t1, t2] <-$-> [t3, t4]
  (<-$->) (TypeArrow t1 t2) (TypeArrow t3 t4) = [t1, t2] <-$-> [t3, t4]
  (<-$->) t1@(TypeComplex name1 deps1) t2@(TypeComplex name2 deps2) = do
    payl <- errSolvePayload
    _    <- if not (name1 == name2)
      then throwError $ UnificationMismatch payl [t1] [t2]
      else return 0
    deps1 <-$-> deps2
  (<-$->) t1 t2 = do
    payl <- errSolvePayload
    throwError $ UnificationFail payl t1 t2

-- | It's useful to bind multiple types at the same time
instance BindableSolve [Type] [Type] where
  (<-$->) []        []        = return emptySubst
  (<-$->) (h1 : t1) (h2 : t2) = do
    uni1 <- h1 <-$-> h2
    uni2 <- (uni1 .> t1) <-$-> (uni1 .> t2)
    return $ uni2 +> uni1
  (<-$->) t1 t2 = do
    payl <- errSolvePayload
    throwError $ UnificationMismatch payl t1 t2

