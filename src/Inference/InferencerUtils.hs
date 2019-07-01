{-|
Module      : Inference.inferencerUtils
Description : Helpful utilities to handle inference
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides basic utilities like fresh name generator, type normalization,
  and tracing helpers.
-}
module Inference.InferencerUtils where

import           Syntax.Base             hiding ( TV
                                                , TypeConstraint
                                                )
import qualified Syntax.Base                   as Syntax

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.ConstraintSolver

import           Data.List                      ( nub )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | Injects current AST annotation into the simplified AST node
addExprAnnot :: Infer SimplifiedExpr -> Infer SimplifiedExpr
addExprAnnot inExpr = do
  s             <- get
  inferTraceTop <-
    return
      $ let InferState { inferTrace = inferTrace } = s
        in  let (h : _) = inferTrace in h
  e <- inExpr
  return $ SimplifiedAnnotated inferTraceTop e

-- | Adds new inference trace node (for debugging purposes)
markTrace :: (Show a, Print a) => a -> Infer ()
markTrace a = do
  s          <- get
  inferTrace <-
    return $ let InferState { inferTrace = inferTrace } = s in inferTrace
  put s { inferTrace = ([(printTree a)] ++ inferTrace) }
  return ()

-- | Removes inference trace node (for debugging purposes)
unmarkTrace :: (Show a, Print a) => a -> Infer ()
unmarkTrace a = do
  s        <- get
  newTrace <-
    return
      $ let InferState { inferTrace = inferTrace } = s in drop 1 inferTrace
  put s { inferTrace = newTrace }
  return ()

-- | Generates error payload for current inference trace (for debugging purposes)
errPayload :: Infer TypeErrorPayload
errPayload = do
  s            <- get
  lastTraceStr <-
    return
      $ let InferState { lastInferExpr = lastInferExpr } = s in lastInferExpr
  return $ TypeErrorPayload lastTraceStr

-- | Performs env ?? name operation but throws error if it fails
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

-- | Generate names for polymoprhic variable names
letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

-- | Generate unique indentificator for variable
freshIdent :: Infer Ident
freshIdent = do
  s <- get
  put s { count = count s + 1 }
  return $ Ident $ "__@$internal_variable__" ++ (letters !! count s) ++ "_"

-- | Generate unique identificator for type variable
freshTypeVar :: Infer Type
freshTypeVar = do
  s <- get
  put s { count = count s + 1 }
  return $ TypeVar $ TV (letters !! count s)

-- | Translates AST node representing "rec" keyword into boolean
isRec :: LetRecKeyword -> Bool
isRec LetRecYes = True
isRec LetRecNo  = False

-- | Injects type check statement into AST
withTypeAnnot
  :: Syntax.TypeConstraint -> ComplexExpression -> Infer ComplexExpression
withTypeAnnot TypeConstrEmpty       e = return e
withTypeAnnot (TypeConstrDef texpr) e = do
  return $ ECChecked e texpr

-- | Injects type containt statement into AST
constraintAnnot :: TypeConstraint -> Infer TypeConstraint
constraintAnnot (TypeConstraint _ constrnt) = do
  payl <- errPayload
  return $ TypeConstraint payl constrnt

-- | Injects type constraint statement into AST for each node in the list
constraintAnnoTypeList :: [TypeConstraint] -> Infer [TypeConstraint]
constraintAnnoTypeList cs = do
  foldrM
    (\c acc -> do
      ca <- constraintAnnot c
      return $ [ca] ++ acc
    )
    []
    cs

-- | Get type scheme for constants
geTypeStaticstScheme :: Constant -> Scheme
geTypeStaticstScheme (CInt    _) = Scheme [] (TypeStatic "Int")
geTypeStaticstScheme (CBool   _) = Scheme [] (TypeStatic "Bool")
geTypeStaticstScheme (CString _) = Scheme [] (TypeStatic "String")

-- | Instantiation operation for types
instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
  as' <- mapM (const freshTypeVar) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ s .> t

-- | Generalization operation for types
generalize :: TypeEnvironment -> Type -> Scheme
generalize env t = Scheme as t
  where as = Set.toList $ freeDimensions t `Set.difference` freeDimensions env

-- | Helper to normalize type free variables
normalizeType _ TypeUnit                = TypeUnit
normalizeType _ (TypeAnnotated v      ) = (TypeAnnotated v)
normalizeType ord (TypeArrow a b        ) = TypeArrow (normalizeType ord a) (normalizeType ord b)
normalizeType ord (TypeTuple a b        ) = TypeTuple (normalizeType ord a) (normalizeType ord b)
normalizeType ord (TypeList   a         ) = TypeList (normalizeType ord a)
normalizeType _ (TypeStatic a         ) = TypeStatic a
normalizeType ord (TypeComplex name deps) = TypeComplex name $ map (normalizeType ord) deps
normalizeType ord (TypeVar a            ) = case Prelude.lookup a ord of
  Just x  -> TypeVar x
  Nothing -> error "Type variable does not exist in type signature"
normalizeType _ v = v

-- | Normalize type free variables
normalize :: Scheme -> Scheme
normalize (Scheme _ body) = Scheme (map snd $ zip (nub $ freeDimensionsList body) (map TV letters)) (normalizeType ord body)
