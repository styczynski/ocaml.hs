{-|
Module      : Inference.TypeExpressionResolver
Description : Resolver for type expressions
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module implements utilities to map AST type expressions into Inference.Types values.
-}
module Inference.TypeExpressionResolver where


import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.Simplifier
import           Inference.ConstraintSolver
import           Inference.InferencerUtils

import           Syntax.Base

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           System.IO.Unsafe

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set


-- | Extracts free variables from type expression and assigns them to fresh
--   type variables.
getTypeSimpleExpressionFV
  :: TypeSimpleExpression -> Infer (Map.Map String TypeVar)
getTypeSimpleExpressionFV (TypeSExprList listType) =
  getTypeExpressionFV listType
getTypeSimpleExpressionFV (TypeSExprIdent _) = return Map.empty
getTypeSimpleExpressionFV TypeSExprEmpty = return Map.empty
getTypeSimpleExpressionFV (TypeSExprAbstract (TypeIdentAbstract name)) = do
  tvv <- freshTypeVar
  return $ let (TypeVar tv) = tvv in Map.singleton name tv

getTypeExpressionFV :: TypeExpression -> Infer (Map.Map String TypeVar)
getTypeExpressionFV (TypeExprSimple simpl) = getTypeSimpleExpressionFV simpl
getTypeExpressionFV (TypeExprIdent (TypeArgJustOne param) _) =
  getTypeSimpleExpressionFV param
getTypeExpressionFV (TypeExprIdent (TypeArgJust firstParam restParams) _) = do
  foldlM
    (\acc (TypeArgEl el) -> do
      r <- getTypeExpressionFV el
      return $ acc `Map.union` r
    )
    Map.empty
    ([firstParam] ++ restParams)
getTypeExpressionFV (TypeFun a b) = do
  t1 <- (getTypeExpressionFV a)
  t2 <- (getTypeExpressionFV b)
  return $ t1 `Map.union` t2
getTypeExpressionFV (TypeExprTuple fstEl restEls) = foldlM
  (\acc el -> do
    t <- (getTypeExpressionFV el)
    return $ acc `Map.union` t
  )
  Map.empty
  ([fstEl] ++ restEls)
getTypeExpressionFV _ = return Map.empty

-- | Extracts free variables from type expression
resolveTypeSimpleExpressionFVNames
  :: TypeSimpleExpression -> Infer (Set.Set String)
resolveTypeSimpleExpressionFVNames TypeSExprEmpty = return $ Set.empty
resolveTypeSimpleExpressionFVNames (TypeSExprList expr) =
  resolveTypeExpressionFVNames expr
resolveTypeSimpleExpressionFVNames (TypeSExprAbstract (TypeIdentAbstract name))
  = return $ Set.singleton name
resolveTypeSimpleExpressionFVNames (TypeSExprIdent _) = return $ Set.empty

resolveTypeExpressionFVNames :: TypeExpression -> Infer (Set.Set String)
resolveTypeExpressionFVNames (TypeExprSimple simpl) =
  resolveTypeSimpleExpressionFVNames simpl
resolveTypeExpressionFVNames (TypeExprIdent (TypeArgJustOne simpl) _) =
  resolveTypeSimpleExpressionFVNames simpl
resolveTypeExpressionFVNames (TypeExprIdent (TypeArgJust firstParam restParams) _)
  = do
    foldlM
      (\acc (TypeArgEl el) -> do
        r <- resolveTypeExpressionFVNames el
        return $ acc `Set.union` r
      )
      Set.empty
      ([firstParam] ++ restParams)
resolveTypeExpressionFVNames (TypeFun a b) = do
  x <- resolveTypeExpressionFVNames a
  y <- resolveTypeExpressionFVNames b
  return $ x `Set.union` y
resolveTypeExpressionFVNames (TypeExprTuple firstElem restElems) = do
  x <- resolveTypeExpressionFVNames firstElem
  foldrM
    (\el acc -> do
      y <- resolveTypeExpressionFVNames el
      return $ acc `Set.union` y
    )
    x
    restElems

resolveTypeSimpleExpressionRec
  :: (Map.Map String TypeVar) -> TypeSimpleExpression -> Infer Type
resolveTypeSimpleExpressionRec fvs TypeSExprEmpty = return TypeUnit
resolveTypeSimpleExpressionRec fvs (TypeSExprIdent (Ident name)) =
  return $ TypeStatic name
resolveTypeSimpleExpressionRec fvs (TypeSExprList expr) = do
  t <- resolveTypeExpressionRec fvs expr
  return $ TypeList t
resolveTypeSimpleExpressionRec fvs (TypeSExprAbstract (TypeIdentAbstract name))
  = do
    parsedName <- return $ [ x | x <- name, not (x `elem` "'") ]
    validFvs   <- return $ name `Map.member` fvs
    if not validFvs
      then do
        payl <- errPayload
        throwError
          $  Debug payl
          $  "Type name "
          ++ name
          ++ " is not a valid polymorhic type name"
      else let (Just tv) = Map.lookup name fvs in return $ TypeVar tv

resolveTypeExpressionRec
  :: (Map.Map String TypeVar) -> TypeExpression -> Infer Type
resolveTypeExpressionRec fvs (TypeExprSimple simpl) =
  resolveTypeSimpleExpressionRec fvs simpl
resolveTypeExpressionRec fvs (TypeExprIdent (TypeArgJust firstParam restParams) (Ident name))
  = do
    typeParams <- foldlM
      (\acc (TypeArgEl expr) -> do
        t <- resolveTypeExpressionRec fvs expr
        return $ [t] ++ acc
      )
      ([])
      ([firstParam] ++ restParams)
    return $ TypeComplex name typeParams
resolveTypeExpressionRec fvs (TypeExprIdent (TypeArgJustOne param) (Ident name))
  = do
    typeParam <- resolveTypeSimpleExpressionRec fvs param
    return $ TypeComplex name [typeParam]
resolveTypeExpressionRec fvs (TypeFun a b) = do
  t1 <- resolveTypeExpressionRec fvs a
  t2 <- resolveTypeExpressionRec fvs b
  return $ TypeArrow t1 t2
resolveTypeExpressionRec fvs (TypeExprTuple fstEl restEls) = do
  tupleT <- foldlM
    (\acc expr -> do
      t <- resolveTypeExpressionRec fvs expr
      return $ TypeTuple t acc
    )
    (TypeTuple TypeUnit TypeUnit)
    ([fstEl] ++ restEls)
  return tupleT

-- | Entrypoint to resolve type expression
resolveTypeExpression :: TypeExpression -> Infer Scheme
resolveTypeExpression exp = do
  fvs  <- getTypeExpressionFV exp
  t    <- resolveTypeExpressionRec fvs exp
  fvsT <- return $ Map.elems fvs
  return $ Scheme fvsT t
