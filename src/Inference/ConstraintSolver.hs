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

data SolveState = SolveState { lastAnnot :: TypeErrorPayload }
initSolve :: SolveState
initSolve = SolveState { lastAnnot = EmptyPayload }

type Solve = StateT (SolveState) (ExceptT TypeError IO)

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

emptySubst :: Subst
emptySubst = Subst $ Map.empty

runSolve :: [TypeConstraint] -> IO (Either TypeError Subst)
runSolve cs = do
  r <- runExceptT (runStateT (solver (emptySubst, cs)) (initSolve))
  case r of
    Left e -> return $ Left e
    Right (s, _) -> return $ Right s

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (su1 .> ts1) (su1 .> ts2)
     return (su2 +> su1)
unifyMany t1 t2 = do
  payl <- errSolvePayload
  throwError $ UnificationMismatch payl t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TypeVar v) t = v `bind` t
unifies t (TypeVar v) = v `bind` t
unifies t1@(TypeComplex name1 deps1) t2@(TypeComplex name2 deps2) = do
  payl <- errSolvePayload
  _ <- if not (name1 == name2) then throwError $ UnificationMismatch payl [t1] [t2] else return 0
  unifyMany deps1 deps2
unifies (TypeList t1) (TypeList t2) = unifyMany [t1] [t2]
unifies (TypeTuple t1 t2) (TypeTuple t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TypeArrow t1 t2) (TypeArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = do
  payl <- errSolvePayload
  throwError $ UnificationFail payl t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((TypeConstraint l (t1, t2)): cs0) -> do
      checkpointAnnotSolve (TypeConstraint l (t1, t2))
      su1  <- unifies t1 t2
      solver (su1 +> su, su1 .> cs0)

bind ::  TypeVar -> Type -> Solve Subst
bind a t | t == TypeVar a     = return emptySubst
         | occursCheck a t = do
                             payl <- errSolvePayload
                             throwError $ InfiniteType payl a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` free t

