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
pullRefStorage name@(Ident nameStr) env =
  let v = getVariable name env in case v of
    RInvalid -> raise $ "Variable " ++ nameStr ++ " does not exist"
    val -> return $ getRefStorage val env

pullVariable :: Ident -> Environment -> Exec RuntimeValue
pullVariable name@(Ident nameStr) env =
  let v = getVariable name env in case v of
    RInvalid -> raise $ "Variable " ++ nameStr ++ " does not exist"
    val -> return $ val

execFunction :: RFunSig -> RFunBody -> [RuntimeValue] -> Exec (RuntimeValue, Environment)
execFunction (RFunSig sig) body args =
  body args

callFunction :: Ident -> [RuntimeValue] -> Environment -> Exec (RuntimeValue, Environment)
callFunction name@(Ident nameStr) args env = do
  def <- pullRefStorage name env
  let argsInCount = length args in
    case def of
      RfFun (RFunSig argsCount) body ->
        if argsInCount < argsCount then
          let fnBody = \paramArgs -> execFunction (RFunSig argsCount) body (args ++ paramArgs) in
            -- throwError ("Create partially applied function " ++ (show argsInCount) ++ (show argsCount))
            return $ newFunction (RFunSig (argsCount - argsInCount)) fnBody env
        else do
          (val, valEnv) <- execFunction (RFunSig argsCount) body args
          return (val, valEnv)
      RfInvalid _ -> raise $ "Reference " ++ nameStr ++ " points nowhere"

newFunction :: RFunSig -> RFunBody -> Environment -> (RuntimeValue, Environment)
newFunction sig body env =
  let (fr, frEnv) = allocRef env in
  ((RRef fr), (setRefStorage fr (RfFun sig body) frEnv))

createFunction :: Ident -> RFunSig -> RFunBody -> Environment -> Environment
createFunction name sig body env =
  let (fr, frEnv) = allocRef env in
  setVariable name (RRef fr) $ setRefStorage fr (RfFun sig body) frEnv

getProgramEnvironmentDefault :: ExecutionResult -> Environment -> Environment
getProgramEnvironmentDefault (Executed _ env) _ = env
getProgramEnvironmentDefault _ envDefault = envDefault