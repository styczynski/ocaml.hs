module Environment where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsSyntax
import Runtime

emptyEnv = Environment { variables=Map.empty }

setVariable :: Ident -> RuntimeValue -> Environment -> Environment
setVariable name val env =
  let Environment { variables = oldVariables } = env in
  env { variables = (Map.insert name val oldVariables) }

setVariables :: [Ident] -> [RuntimeValue] -> Environment -> Environment
setVariables names vals env = foldl (\env (n,v) -> setVariable n v env) env (zip names vals)

delVariable :: Ident -> Environment -> Environment
delVariable name env =
  let Environment { variables = oldVariables } = env in
  env { variables = (Map.delete name oldVariables) }

getVariable :: Ident -> Environment -> RuntimeValue
getVariable name env =
  let Environment { variables = oldVariables } = env in
  Map.findWithDefault RInvalid name oldVariables

pullVariable :: Ident -> Environment -> Exec RuntimeValue
pullVariable name env =
  let v = getVariable name env in case v of
    RInvalid -> throwError "Missing runtime value"
    val -> return $ val