module InterpreterPatterns where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import Runtime
import Environment
import AbsSyntax


getPatternMapping :: SimplePattern -> RuntimeValue -> Map.Map Ident RuntimeValue
getPatternMapping (PatConst _) _ = Map.empty
getPatternMapping (PatCons hPat tPat) (RList (h:t)) =
   Map.unionWith (\_ b -> b) (getPatternMapping hPat h) (getPatternMapping tPat $ RList t)
getPatternMapping PatNone _ = Map.empty
getPatternMapping (PatTuple (PTuple firstElement tupleElements)) (RTuple vals) =
  fst $ foldl (\(accMap,accVals) (PTupleElement elem) ->
      let submap = getPatternMapping elem (head accVals) in
        ((Map.unionWith (\_ b -> b) accMap submap),(tail accVals))) (Map.empty, vals) (firstElement : tupleElements)
getPatternMapping (PatIdent name) val = Map.insert name val Map.empty
getPatternMapping (PatList (PList elems)) (RList vals) =
  fst $ foldl (\(accMap,accVals) (PListElement elem) ->
    let submap = getPatternMapping elem (head accVals) in
      ((Map.unionWith (\_ b -> b) accMap submap),(tail accVals))) (Map.empty, vals) elems

setPattern :: SimplePattern -> RuntimeValue -> Environment -> Environment
setPattern pattern val env = let pm = getPatternMapping pattern val in
  Map.foldrWithKey (\name val env -> setVariable name val env) env pm

setPatterns :: [SimplePattern] -> [RuntimeValue] -> Environment -> Environment
setPatterns patterns vals env = foldl (\env (p,v) -> setPattern p v env) env (zip patterns vals)