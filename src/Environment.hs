module Environment where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsSyntax
import Runtime

emptyEnv = Environment { variables=Map.empty, definitions=Map.empty }

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

getDefinition :: Ident -> Environment -> RuntimeDefinition
getDefinition name env =
  let Environment { definitions = oldDefinitions } = env in
   Map.findWithDefault RDInvalid name oldDefinitions

pullDefinition :: Ident -> Environment -> Exec RuntimeDefinition
pullDefinition name env =
 let v = getDefinition name env in case v of
   RDInvalid -> throwError "Missing runtime definition"
   val -> return $ val

execFunction :: RFunSig -> RFunBody -> [RuntimeValue] -> Environment -> Exec RuntimeValue
execFunction (RFunSig sig) body args env =
  body args

callFunction :: Ident -> [RuntimeValue] -> Environment -> Exec RuntimeValue
callFunction name args env = do
  def <- pullDefinition name env
  case def of
    RDFun sig body -> execFunction sig body args env
    _ -> throwError "Object is not callable"

createFunction :: Ident -> RFunSig -> RFunBody -> Environment -> Environment
createFunction name sig body env =
  let Environment { definitions = oldDefinitions } = env in
   env { definitions = (Map.insert name (RDFun sig body) oldDefinitions) }