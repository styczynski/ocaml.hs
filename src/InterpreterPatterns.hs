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
getPatternMapping (PatIdent name) val = Map.insert name val Map.empty

setPattern :: SimplePattern -> RuntimeValue -> Environment -> Environment
setPattern pattern val env = let pm = getPatternMapping pattern val in
  Map.foldrWithKey (\name val env -> setVariable name val env) env pm

setPatterns :: [SimplePattern] -> [RuntimeValue] -> Environment -> Environment
setPatterns patterns vals env = foldl (\env (p,v) -> setPattern p v env) env (zip patterns vals)