module Environment where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsSyntax
import Runtime

emptyEnv = Environment {
  variables = Map.empty,
  refs = Map.empty,
  defs = Map.empty,
  nextFreeRef = 0
}

allocRef :: Environment -> (Integer, Environment)
allocRef env =
  let Environment { nextFreeRef=freeRef } = env in
  (freeRef, (env { nextFreeRef = freeRef + 1 }))

setNativeVariable :: (PackableValue a) => String -> a -> Exec (RuntimeValue, Environment)
setNativeVariable name val = do
  (val1, env1) <- packVal val
  env2 <- return $ setVariable (Ident name) val1 env1
  return (val1, env2)

setVariable :: Ident -> RuntimeValue -> Environment -> Environment
setVariable name val env =
  let Environment { variables = oldVariables } = delVariable name $ delDef name env in
  env { variables = (Map.insert name val oldVariables) }

setVariables :: [Ident] -> [RuntimeValue] -> Environment -> Environment
setVariables names vals env = foldl (\env (n,v) -> setVariable n v env) env (zip names vals)

defContainsField :: Ident -> RuntimeDef -> Bool
defContainsField name (DRecord _ fieldName) = fieldName == name
defContainsField name _ = False

findRecordDefByFieldNames :: [Ident] -> Environment -> RuntimeDef
findRecordDefByFieldNames (name:_) env =
  let Environment { defs = defs } = env in
  case Map.elems (Map.filter (defContainsField name) defs) of
    [] -> DInvalid
    (h:t) -> h

setDef :: Ident -> RuntimeDef -> Environment -> Environment
setDef name def env =
  let Environment { defs = defs } = env in
  env { defs = (Map.insert name def defs) }

getDef :: Ident -> Environment -> RuntimeDef
getDef name env =
  let Environment { defs = defs } = env in
  Map.findWithDefault DInvalid name defs

delDef :: Ident -> Environment -> Environment
delDef name env =
  let Environment { defs = defs } = env in
  env { defs = (Map.delete name defs) }

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

callOperator :: Ident -> Integer -> [RuntimeValue] -> Exec (RuntimeValue, Environment)
callOperator name@(Ident nameStr) priority args = do
  env <- ask
  opFn <- (case getDef name env of
    DInvalid -> raise $ "Called operator do not exist: " ++ nameStr
    (DOperator opName opPrio opBody) ->
      if (opName == name && opPrio == priority) then
      return $ opBody
      else raise $ "Invalid operator called: " ++ nameStr
    _ -> raise $ "Could not call: "++ nameStr ++ " it's not an operator. ")
  opFn args

callOperatorF :: OperatorF -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorF (OperatorF op) arg1 = callOperator (Ident op) 5 [arg1]

callOperatorE :: OperatorE -> RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorE (OperatorE op) arg1 arg2 = callOperator (Ident op) 4 [arg1, arg2]

callOperatorD :: OperatorD -> RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorD (OperatorD op) arg1 arg2 = callOperator (Ident op) 3 [arg1, arg2]

callOperatorDS :: RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorDS arg1 arg2 = callOperator (Ident "*") 3 [arg1, arg2]

callOperatorC :: OperatorC -> RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorC (OperatorC op) arg1 arg2 = callOperator (Ident op) 2 [arg1, arg2]

callOperatorB :: OperatorB -> RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorB (OperatorB op) arg1 arg2 = callOperator (Ident op) 1 [arg1, arg2]

callOperatorA :: OperatorA -> RuntimeValue -> RuntimeValue -> Exec (RuntimeValue, Environment)
callOperatorA (OperatorA op) arg1 arg2 = callOperator (Ident op) 0 [arg1, arg2]