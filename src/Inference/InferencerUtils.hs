module Inference.InferencerUtils where

import Syntax.Base hiding (TV)

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Type
import Inference.Substitutions
import Inference.Errors
import Inference.ConstraintSolver

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

addExprAnnot :: Infer Expr -> Infer Expr
addExprAnnot inExpr = do
  s <- get
  inferTraceTop <- return $ let InferState { inferTrace = inferTrace } = s in let (h:_) = inferTrace in h
  e <- inExpr
  return $ Annot inferTraceTop e

markTrace :: (Show a, Print a) => a -> Infer ()
markTrace a = do
  s <- get
  inferTrace <- return $ let InferState { inferTrace = inferTrace } = s in inferTrace
  put s{ inferTrace = ([(printTree a)] ++ inferTrace) }
  return ()

unmarkTrace :: (Show a, Print a) => a -> Infer ()
unmarkTrace a = do
  s <- get
  newTrace <- return $ let InferState { inferTrace = inferTrace } = s in drop 1 inferTrace
  put s{ inferTrace = newTrace }
  return ()

errPayload :: Infer TypeErrorPayload
errPayload = do
  s <- get
  lastTraceStr <- return $ let InferState { lastInferExpr = lastInferExpr } = s in lastInferExpr
  return $ TypeErrorPayload lastTraceStr

-- | Lookup type in the environment
lookupEnv :: Ident -> Infer Type
lookupEnv name = do
  env <- ask
  case (env ?? name) of
      Nothing -> do
        payl <- errPayload
        throwError $ UnboundVariable payl name
      (Just scheme) -> do
        t <- instantiate scheme
        return t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

freshIdent :: Infer Ident
freshIdent = do
    s <- get
    put s{count = count s + 1}
    return $ Ident $ "__@$internal_variable__" ++ (letters !! count s) ++ "_"

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

isRec :: LetRecKeyword -> Bool
isRec LetRecYes = True
isRec LetRecNo = False

withTypeAnnot :: TypeConstraint -> ComplexExpression -> Infer ComplexExpression
withTypeAnnot TypeConstrEmpty e = return e
withTypeAnnot (TypeConstrDef texpr) e = do
  return $ ECChecked e texpr

constraintAnnot :: Constraint -> Infer AConstraint
constraintAnnot constrnt = do
  payl <- errPayload
  return $ AConstraint payl constrnt

constraintAnnotList :: [Constraint] -> Infer [AConstraint]
constraintAnnotList cs = do
  foldrM (\c acc -> do
    ca <- constraintAnnot c
    return $ [ca] ++ acc)  [] cs

getConstScheme :: Constant -> Scheme
getConstScheme (CInt _) = Forall [] (TCon "Int")
getConstScheme (CBool _) = Forall [] (TCon "Bool")
getConstScheme (CString _) = Forall [] (TCon "String")

closeOver :: Type -> Scheme
closeOver = normalize . generalize Inference.TypingEnvironment.empty

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ s .> t

generalize :: Env -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ free t `Set.difference` free env

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TList a)  = fv a
    fv (TTuple a b) = fv a ++ fv b
    fv TUnit = []
    fv (TExport _) = []
    fv (TCon _)   = []
    fv (TDep _ deps) = foldl (\acc el -> acc ++ (fv el)) [] deps

    normtype TUnit = TUnit
    normtype (TExport v) = (TExport v)
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TTuple a b) = TTuple (normtype a) (normtype b)
    normtype (TList a) = TList (normtype a)
    normtype (TCon a)   = TCon a
    normtype (TDep name deps) = TDep name $ map normtype deps
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
