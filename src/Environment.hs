module Environment where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsSyntax
import Runtime

emptyEnv = Environment { variables=Map.empty, refs=Map.empty, nextFreeRef=0 }

allocRef :: Environment -> (Integer, Environment)
allocRef env =
  let Environment { nextFreeRef=freeRef } = env in
  (freeRef, (env { nextFreeRef = freeRef + 1 }))

setVariable :: Ident -> RuntimeValue -> Environment -> Environment
setVariable name val env =
  let Environment { variables = oldVariables } = delVariable name env in
  env { variables = (Map.insert name val oldVariables) }

setVariables :: [Ident] -> [RuntimeValue] -> Environment -> Environment
setVariables names vals env = foldl (\env (n,v) -> setVariable n v env) env (zip names vals)

setRefStorage :: Integer -> RuntimeRefValue -> Environment -> Environment
setRefStorage id val env =
  let Environment { refs = oldRefs } = env in
  env { refs = (Map.insert id val oldRefs) }

getRefStorage :: RuntimeValue -> Environment -> RuntimeRefValue
getRefStorage (RRef id) env =
  let Environment { refs = oldRefs } = env in
  Map.findWithDefault (RfInvalid RInvalid) id oldRefs
getRefStorage val env = RfInvalid val

delRefStorage :: Integer -> Environment -> Environment
delRefStorage id env =
  let Environment { refs = oldRefs } = env in
  env { refs = (Map.delete id oldRefs) }

delVariable :: Ident -> Environment -> Environment
delVariable name env =
  let Environment { variables = oldVariables } = env in
  case Map.lookup name oldVariables of
    Just (RRef ref) -> delRefStorage ref $ env { variables = (Map.delete name oldVariables) }
    _ -> env { variables = (Map.delete name oldVariables) }

getVariable :: Ident -> Environment -> RuntimeValue
getVariable name env =
  let Environment { variables = oldVariables } = env in
  Map.findWithDefault RInvalid name oldVariables

pullRefStorage :: Ident -> Environment -> Exec RuntimeRefValue
pullRefStorage name env =
  let v = getVariable name env in case v of
    RInvalid -> throwError "Missing runtime value"
    val -> return $ getRefStorage val env

pullVariable :: Ident -> Environment -> Exec RuntimeValue
pullVariable name env =
  let v = getVariable name env in case v of
    RInvalid -> throwError "Missing runtime value"
    val -> return $ val

execFunction :: RFunSig -> RFunBody -> [RuntimeValue] -> Environment -> Exec RuntimeValue
execFunction (RFunSig sig) body args env =
  body args

callFunction :: Ident -> [RuntimeValue] -> Environment -> Exec RuntimeValue
callFunction name args env = do
  def <- pullRefStorage name env
  case def of
    RfFun sig body -> execFunction sig body args env
    _ -> throwError "Object is not callable"

createFunction :: Ident -> RFunSig -> RFunBody -> Environment -> Environment
createFunction name sig body env =
  let (fr, frEnv) = allocRef env in
  setVariable name (RRef fr) $ setRefStorage fr (RfFun sig body) frEnv