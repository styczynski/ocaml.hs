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
runSolve :: [TypeConstraint] -> IO (Either TypeError TypeSubstitution)
runSolve cs = do
  r <- runExceptT (runStateT (solver (TypeUnifier cs emptySubst)) emptySolverState)
  case r of
    Left  e      -> return $ Left e
    Right (s, _) -> return $ Right s

-- | Runs solver to unify all types
solver :: TypeUnifier -> Solve TypeSubstitution
solver (TypeUnifier cs su) = case cs of
  [] -> return su
  ((TypeConstraint l (typeArgA, typeArgB)) : cs0) -> do
    checkpointAnnotSolve (TypeConstraint l (typeArgA, typeArgB))
    su1 <- typeArgA <-$-> typeArgB
    solver $ TypeUnifier (su1 .> cs0) (su1 +> su)

-- | Represents a data type that can bind values of one type to the other one
class BindableSolve a b where
  (<-$->) :: a -> b -> Solve TypeSubstitution

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
  (<-$->) typeArgA typeArgB | typeArgA == typeArgB                    = return emptySubst
  (<-$->) (TypeVar v)       t                 = v <-$-> t
  (<-$->) t                 (TypeVar  v     ) = v <-$-> t
  (<-$->) (TypeList typeArgA    ) (TypeList typeArgB    ) = [typeArgA] <-$-> [typeArgB]
  (<-$->) (TypeTuple typeArgA typeArgB) (TypeTuple typeArgC typeArgD) = [typeArgA, typeArgB] <-$-> [typeArgC, typeArgD]
  (<-$->) (TypeArrow typeArgA typeArgB) (TypeArrow typeArgC typeArgD) = [typeArgA, typeArgB] <-$-> [typeArgC, typeArgD]
  (<-$->) typeArgA@(TypePoly alternatives1) typeArgB@(TypePoly alternatives2) =
    alternatives1 <-$-> alternatives2
  (<-$->) typeArgA@(TypeComplex name1 deps1) typeArgB@(TypeComplex name2 deps2) = do
    payl <- errSolvePayload
    _    <- if not (name1 == name2)
      then throwError $ UnificationMismatch payl [typeArgA] [typeArgB]
      else return 0
    deps1 <-$-> deps2
  (<-$->) typeArgA typeArgB = do
    payl <- errSolvePayload
    throwError $ UnificationFail payl typeArgA typeArgB

-- | It's useful to bind multiple types at the same time
instance BindableSolve [Type] [Type] where
  (<-$->) []        []        = return emptySubst
  (<-$->) (h1 : typeArgA) (h2 : typeArgB) = do
    uni1 <- h1 <-$-> h2
    uni2 <- (uni1 .> typeArgA) <-$-> (uni1 .> typeArgB)
    return $ uni2 +> uni1
  (<-$->) typeArgA typeArgB = do
    payl <- errSolvePayload
    throwError $ UnificationMismatch payl typeArgA typeArgB

