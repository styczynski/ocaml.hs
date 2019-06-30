module Inference.ConstraintSolver where

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Substitutions
import Inference.Errors
import Inference.Type

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Solve state
data SolveState = SolveState { lastAnnot :: TypeErrorPayload }
initSolve :: SolveState
initSolve = SolveState { lastAnnot = EmptyPayload }


-- | Constraint solver monad
type Solve a = (StateT
                 SolveState
                 (Except
                   TypeError))
                 a

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

checkpointAnnotSolve :: AConstraint -> Solve ()
checkpointAnnotSolve (AConstraint l _) = do
    s <- get
    put s{lastAnnot = l}
    return ()

errSolvePayload :: Solve TypeErrorPayload
errSolvePayload = do
  s <- get
  lastAnnot <- return $ let SolveState { lastAnnot = lastAnnot } = s in lastAnnot
  return lastAnnot

-- | The empty substitution
emptySubst :: Subst
emptySubst = Subst $ Map.empty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [AConstraint] -> Either TypeError Subst
runSolve cs = let st = (emptySubst, cs) in
  case runExcept $ runStateT (solver st) initSolve of
    Left e -> Left e
    Right (s, _) -> Right s

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = do
  payl <- errSolvePayload
  throwError $ UnificationMismatch payl t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies t1@(TDep name1 deps1) t2@(TDep name2 deps2) = do
  payl <- errSolvePayload
  _ <- if not (name1 == name2) then throwError $ UnificationMismatch payl [t1] [t2] else return 0
  unifyMany deps1 deps2
unifies (TList t1) (TList t2) = unifyMany [t1] [t2]
unifies (TTuple t1 t2) (TTuple t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = do
  payl <- errSolvePayload
  throwError $ UnificationFail payl t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((AConstraint l (t1, t2)): cs0) -> do
      checkpointAnnotSolve (AConstraint l (t1, t2))
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = do
                             payl <- errSolvePayload
                             throwError $ InfiniteType payl a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

