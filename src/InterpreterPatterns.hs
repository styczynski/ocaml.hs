module InterpreterPatterns where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Foldable

import Runtime
import Environment
import AbsSyntax

getPatternMapping :: SimplePattern -> RuntimeValue -> Exec (Map.Map Ident RuntimeValue)
getPatternMapping (PatConst _) _ = return $ Map.empty
getPatternMapping (PatCons hPat tPat) (RList (h:t)) = do
   e1 <- getPatternMapping hPat h
   e2 <- getPatternMapping tPat $ RList t
   return $ Map.unionWith (\_ b -> b) e1 e2
getPatternMapping PatNone _ = return $ Map.empty
getPatternMapping (PatTuple (PTuple firstElement tupleElements)) (RTuple vals) = do
  _ <- if (length vals) == 1+(length tupleElements) then return REmpty else raise $ "Matching failed. Length do not match."
  (newEnv, _) <- foldlM (\(accMap,accVals) (PTupleElement elem)-> do
      submap <- getPatternMapping elem (head accVals)
      return $ ((Map.unionWith (\_ b -> b) accMap submap),(tail accVals))) (Map.empty, vals) (firstElement : tupleElements)
  return newEnv
getPatternMapping (PatIdent name) val = do
  env <- ask
  nameDef <- return $ getDef name env
  case (nameDef, val) of
    (DInvalid, val) -> return $ Map.insert name val Map.empty
    (dval@(DVariant varName optName), (RVariant rName rOpt REmpty)) -> if (varName == rName && optName == rOpt) then
        return $ Map.insert name val Map.empty
      else
        raise $ "Could not match two variant types: " ++ (getTypeStr (val, env))
    _ -> raise $ "Match pattern failed"
getPatternMapping (PatConstr typeConstr patternOption) val@(RVariant optionVar option optionVal) = do
  env <- ask
  nameDef <- return $ getDef typeConstr env
  case nameDef of
    DInvalid -> raise $ "Constructor for data " ++ (treeToStr typeConstr) ++ " do not exist, matching failed."
    dvar@(DVariant varName optName) ->
      -- TODO: Fix This ugly error message
      if (optionVar == varName && optName == option && typeConstr == option) then
        getPatternMapping patternOption optionVal
      else raise $ "Constructor type match failed. Tried to match " ++ ("") ++ " with value of type " ++ (show val)
getPatternMapping (PatList (PList elems)) (RList vals) = do
  _ <- if (length vals) == (length elems) then return REmpty else raise $ "Matching failed. Length do not match."
  (newEnv, _) <- foldrM (\(PListElement elem) (accMap,accVals) -> do
    submap <- getPatternMapping elem (head accVals)
    return $ ((Map.unionWith (\_ b -> b) accMap submap),(tail accVals))) (Map.empty, vals) elems
  return newEnv
getPatternMapping pattern value = raise $ "Pattern matching error. Could not match " ++ (treeToStr pattern) ++ " with " ++ (valueToStr value)

setPattern :: SimplePattern -> RuntimeValue -> Environment -> Exec Environment
setPattern pattern val env = do
  pm <- getPatternMapping pattern val
  return $ Map.foldrWithKey (\name val env -> setVariable name val env) env pm

setPatterns :: [SimplePattern] -> [RuntimeValue] -> Environment -> Exec Environment
setPatterns patterns vals env = foldrM (\(p,v) env -> setPattern p v env) env (zip patterns vals)

--setPatternsIfCan :: [SimplePattern] -> [RuntimeValue] -> Environment -> Exec (Environment, SimplePattern, Bool)
--setPatternsIfCan patterns vals env = do {
--    newEnv <- setPatterns patterns vals env ;
--    return (newEnv, True)
--  } `catchError` (\err -> return (env, False))

setMatchPatterns :: [(SimplePattern, a)] -> RuntimeValue -> Environment -> Exec (Environment, Maybe a)
setMatchPatterns patterns val env = do
  (newEnv, selVal) <- foldrM (\(pat, selVal) (env, sel) -> do {
      newEnv <- setPattern pat val env ;
      return (newEnv, Just selVal)
    } `catchError` (\err -> return (env, sel))) (env, Nothing) patterns
  return (newEnv, selVal)