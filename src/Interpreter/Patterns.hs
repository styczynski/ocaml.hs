{-|
Module      : Interpreter.Patterns
Description : Code to handle destructing patterns
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module exports functions to parse destructing assignments and matching clauses.
-}
module Interpreter.Patterns where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import           Data.Foldable

import           Runtime.Runtime
import           Runtime.Environment

import           Interop.Arithmetics

import           Syntax.Base

constToVal :: Constant -> RuntimeValue
constToVal (CInt    val    ) = RInt val
constToVal (CString val    ) = RString val
constToVal (CBool   CBTrue ) = RBool True
constToVal (CBool   CBFalse) = RBool False

patternMapUnion
  :: (Map.Map Ident RuntimeValue)
  -> (Map.Map Ident RuntimeValue)
  -> Exec (Map.Map Ident RuntimeValue)
patternMapUnion e1 e2 = do
  ret       <- return $ Map.unionWith (\_ _ -> RInternalDupIdent) e1 e2
  badAssocs <- return $ filter (\(_, v) -> v == RInternalDupIdent) $ Map.assocs
    ret
  badAssocsNames <- return $ foldr
    (\((Ident k), _) acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ k
    )
    ""
    badAssocs
  _ <- if (length badAssocs) > 0
    then raise $ "Duplicated variable names in patterns: " ++ badAssocsNames
    else return REmpty
  return ret

getPatternExportNames :: SimplePattern -> [String]
getPatternExportNames (PatConst c) = []
getPatternExportNames (PatCons hPat tPat) =
  (getPatternExportNames hPat) ++ (getPatternExportNames tPat)
getPatternExportNames PatNone = []
getPatternExportNames (PatTuple (PTuple firstElement tupleElements)) = do
  foldr (\(PTupleElement el) acc -> (getPatternExportNames el) ++ acc)
        []
        ([firstElement] ++ tupleElements)
getPatternExportNames (PatIdent (Ident name)) = [name]
getPatternExportNames (PatOr firstAlt restAlts) = do
  foldr (\(PatOrExpr pat) acc -> (getPatternExportNames pat) ++ acc) [] ([firstAlt] ++ restAlts)
getPatternExportNames (PatTag _ (TagPatSome patternOption)) =
  getPatternExportNames patternOption
getPatternExportNames (PatTag _ TagPatNone) = []
getPatternExportNames (PatConstr typeConstr patternOption) =
  getPatternExportNames patternOption
getPatternExportNames (PatList (PList elems)) = do
  foldr (\(PListElement el) acc -> (getPatternExportNames el) ++ acc) [] (elems)
getPatternExportNames pattern = []

getPatternsExportNames :: [SimplePattern] -> [String]
getPatternsExportNames l =
  foldr (\el acc -> (getPatternExportNames el) ++ acc) [] l

allDifferent []       = True
allDifferent (x : xs) = x `notElem` xs && allDifferent xs

validatePatterns :: [SimplePattern] -> Exec ()
validatePatterns l = do
  names          <- return $ getPatternsExportNames l
  badAssocsNames <- return $ foldr
    (\k acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ k)
    ""
    names
  uniqueCond <- return $ allDifferent names
  _          <- if uniqueCond
    then return REmpty
    else raise $ "Duplicated variable names in patterns: " ++ badAssocsNames
  return ()

getPatternMapping
  :: SimplePattern -> RuntimeValue -> Exec (Map.Map Ident RuntimeValue)
getPatternMapping (PatConst c) v = do
  r    <- return $ constToVal c
  isEq <- valueEq r v
  _    <- if isEq
    then return REmpty
    else raise $ "Matching failed. Value is not equal to the constant"
  return Map.empty
getPatternMapping (PatCons hPat tPat) (RList (h : t)) = do
  e1 <- getPatternMapping hPat h
  e2 <- getPatternMapping tPat $ RList t
  patternMapUnion e1 e2
getPatternMapping PatNone _ = return $ Map.empty
getPatternMapping (PatTuple (PTuple firstElement tupleElements)) (RTuple vals)
  = do
    _ <- if (length vals) == 1 + (length tupleElements)
      then return REmpty
      else raise $ "Matching failed. Length do not match."
    (newEnv, _) <- foldlM
      (\(accMap, accVals) (PTupleElement elem) -> do
        submap       <- getPatternMapping elem (head accVals)
        submapMerged <- patternMapUnion accMap submap
        return $ (submapMerged, (tail accVals))
      )
      (Map.empty, vals)
      (firstElement : tupleElements)
    return newEnv
getPatternMapping (PatIdent name) val = do
  env     <- ask
  nameDef <- return $ getDef name env
  case (nameDef, val) of
    (DInvalid, val) -> return $ Map.insert name val Map.empty
    (dval@(DVariant varName optName), (RVariant rName rOpt REmpty)) ->
      if (varName == rName && optName == rOpt)
        then return $ Map.insert name val Map.empty
        else
          raise
          $  "Could not match two variant types: "
          ++ (getTypeStr (val, env))
    _ -> raise $ "Match pattern failed"
getPatternMapping (PatConstr typeConstr patternOption) val@(RVariant optionVar option optionVal)
  = do
    env     <- ask
    nameDef <- return $ getDef typeConstr env
    case nameDef of
      DInvalid ->
        raise
          $  "Constructor for data "
          ++ (treeToStr typeConstr)
          ++ " do not exist, matching failed."
      dvar@(DVariant varName optName) ->
        -- TODO: Fix This ugly error message
        if (optionVar == varName && optName == option && typeConstr == option)
          then getPatternMapping patternOption optionVal
          else
            raise
            $  "Constructor type match failed. Tried to match "
            ++ ("")
            ++ " with value of type "
            ++ (show val)
getPatternMapping (PatOr firstAlt restAlts) val = do
  mapping <- foldrM
      (\(PatOrExpr pat) retMap ->
        do
            t <- getPatternMapping pat val
            return t
          `catchError` (\err -> return retMap)
      )
      (Map.empty)
      ([firstAlt] ++ restAlts)
  return mapping
getPatternMapping (PatTag (Ident name) (TagPatSome patternOption)) val@(RTag tagName tagVal)
  = if (name == tagName)
      then getPatternMapping patternOption tagVal
      else
        raise
        $  "Tag match failed. Tried to match "
        ++ ("")
        ++ " with value of type "
        ++ (show val)
getPatternMapping (PatList (PList elems)) (RList vals) = do
  _ <- if (length vals) == (length elems)
    then return REmpty
    else raise $ "Matching failed. Length do not match."
  (newEnv, _) <- foldrM
    (\(PListElement elem) (accMap, accVals) -> do
      submap       <- getPatternMapping elem (head accVals)
      submapMerged <- patternMapUnion accMap submap
      return $ (submapMerged, (tail accVals))
    )
    (Map.empty, vals)
    elems
  return newEnv
getPatternMapping pattern value = do
  env <- ask
  raise
    $  "Pattern matching error. Could not match "
    ++ (treeToStr pattern)
    ++ " with "
    ++ (valueToStr env value)

setPattern :: SimplePattern -> RuntimeValue -> Environment -> Exec Environment
setPattern pattern val env = do
  pm <- getPatternMapping pattern val
  return $ Map.foldrWithKey (\name val env -> setVariable name val env) env pm

setPatterns
  :: [SimplePattern] -> [RuntimeValue] -> Environment -> Exec Environment
setPatterns patterns vals env = do
  validatePatterns patterns
  foldrM (\(p, v) env -> setPattern p v env) env (zip patterns vals)

setMatchPatterns
  :: [(SimplePattern, a)]
  -> RuntimeValue
  -> Environment
  -> Exec (Environment, Maybe a)
setMatchPatterns patterns val env = do
  (newEnv, selVal) <- foldrM
    (\(pat, selVal) (env, sel) ->
      do
          newEnv <- setPattern pat val env
          return (newEnv, Just selVal)
        `catchError` (\err -> return (env, sel))
    )
    (env, Nothing)
    patterns
  return (newEnv, selVal)
