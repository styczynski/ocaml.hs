module Environment where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import Env
import AbsSyntax
import Runtime
import Infer

emptyEnv = Environment {
  variables = Map.empty,
  refs = Map.empty,
  defs = Map.empty,
  nextFreeRef = 0,
  itypes = empty,
  stypes = initInfer
}

mergeEnv :: Environment -> Environment -> Environment
mergeEnv envA envB = envB

importEnvRef :: (Integer, Environment) -> Ident -> Environment -> Environment -> Environment
importEnvRef (fr, _) name envA envB = importEnv name (RRef fr) envA envB

importEnv :: Ident -> RuntimeValue -> Environment -> Environment -> Environment
importEnv name val envA envB =
  let Environment { variables = variables  } = envA in
  envA { variables = (variables `Map.union` (Map.singleton name val)) }

importVRef :: (Integer, Environment) -> Ident -> Environment -> Exec (a, Environment) -> Exec (a, Environment)
importVRef fr name envA v = do
  (val, envB) <- v
  return (val, (importEnvRef fr name envA envB))

setTypesState :: InferState -> Environment -> Environment
setTypesState e env =
  env { stypes = e }

getTypesState :: Environment -> InferState
getTypesState env =
  let Environment { stypes = stypes } = env in
  stypes

setTypesEnv :: Env -> Environment -> Environment
setTypesEnv e env =
  env { itypes = e }

getTypesEnv :: Environment -> Env
getTypesEnv env =
  let Environment { itypes = itypes } = env in
  itypes

shadowEnv :: Environment -> Environment -> Environment
shadowEnv envA envB =
  let Environment { nextFreeRef = nextFreeRef, refs = refs } = envB in
  envA { nextFreeRef = nextFreeRef, refs = refs }

shadowM :: Environment -> Exec Environment -> Exec Environment
shadowM envA v = do
  envB <- v
  return $ shadowEnv envA envB

shadow :: Environment -> Exec (a, Environment) -> Exec (a, Environment)
shadow envA v = do
  (val, envB) <- v
  return (val, (shadowEnv envA envB))

(->>) :: Environment -> Exec (a, Environment) -> Exec (a, Environment)
(->>) = shadow

allocRef :: Environment -> (Integer, Environment)
allocRef env =
  let Environment { nextFreeRef=freeRef } = env in
  (freeRef, (env { nextFreeRef = freeRef + 1 }))

setNativeUntypedVariable :: (PackableValue a) => String -> a -> Exec (RuntimeValue, Environment)
setNativeUntypedVariable name val = do
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

getVariable :: Ident -> Environment -> Exec (RuntimeValue, Environment)
getVariable name env =
  let Environment { variables = oldVariables } = env in
  case Map.findWithDefault RInvalid name oldVariables of
    RInvalid -> return (RInvalid, env)
    (RRef fr) -> case getRefStorage (RRef fr) env of
      fn@(RfFun (RFunSig 0) body) -> do
        callFunctionF fn [] env
      _ -> return ((RRef fr), env)
    x -> return (x, env)

pullRefStorage :: Ident -> Environment -> Exec (RuntimeRefValue, Environment)
pullRefStorage name@(Ident nameStr) env = do
  (v, env2) <- getVariable name env
  case v of
    RInvalid -> raise $ "Variable " ++ nameStr ++ " does not exist"
    val -> return (getRefStorage val env, env2)

pullVariable :: Ident -> Environment -> Exec (RuntimeValue, Environment)
pullVariable name@(Ident nameStr) env = do
  (v, env2) <- getVariable name env
  case v of
    RInvalid -> raise $ "Variable " ++ nameStr ++ " does not exist"
    val -> return $ (val, env2)

execFunction :: RFunSig -> RFunBody -> [RuntimeValue] -> Exec (RuntimeValue, Environment)
execFunction (RFunSig sig) body args = do
  env <- ask
  shadow env $ body args

callFunctionF :: RuntimeRefValue -> [RuntimeValue] -> Environment -> Exec (RuntimeValue, Environment)
callFunctionF (RfFun (RFunSig argsCount) body) args env =
  let argsInCount = length args in
    if (argsCount > 0) && (argsInCount < argsCount) then
      let fnBody = \paramArgs -> execFunction (RFunSig argsCount) body (args ++ paramArgs) in
        shadow env $ return $ newFunction (RFunSig (argsCount - argsInCount)) fnBody env
    else
      if argsInCount > argsCount then do
        (val, valEnv) <- execFunction (RFunSig argsCount) body (take argsCount args)
        fnCont <- return $ getRefStorage val valEnv
        (val, valEnv) <- local (\_ -> valEnv) $ callFunctionF fnCont (drop (argsInCount - argsCount) args) valEnv
        return (val, valEnv)
      else do
        (val, valEnv) <- shadow env $ execFunction (RFunSig argsCount) body args
        return (val, valEnv)
callFunctionF _ _ _ = raise $ "Could not call non-functional value"

callFunctionR :: RuntimeValue -> [RuntimeValue] -> Environment -> Exec (RuntimeValue, Environment)
callFunctionR val args env = do
  def <- return $ getRefStorage val env
  let argsInCount = length args in
    case def of
      RfFun sig body -> callFunctionF (RfFun sig body) args env
      RfInvalid _ -> raise $ "Reference points nowhere"

callFunction :: Ident -> [RuntimeValue] -> Environment -> Exec (RuntimeValue, Environment)
callFunction name@(Ident nameStr) args env = do
  (def, env2) <- pullRefStorage name env
  let argsInCount = length args in
    case def of
      RfFun sig body -> callFunctionF (RfFun sig body) args env2
      RfInvalid _ -> raise $ "Reference " ++ nameStr ++ " points nowhere"

newFunction :: RFunSig -> RFunBody -> Environment -> (RuntimeValue, Environment)
newFunction sig body env =
  let (fr, frEnv) = allocRef env in
  ((RRef fr), (setRefStorage fr (RfFun sig body) frEnv))

createFunction :: Ident -> Maybe (Integer, Environment) -> RFunSig -> RFunBody -> Environment -> Environment
createFunction name (Just newRef) sig body env =
  let (fr, frEnv) = newRef in
  setVariable name (RRef fr) $ setRefStorage fr (RfFun sig (\args -> do
    local (\e -> setVariable name (RRef fr) e) $ body args)) frEnv
createFunction name Nothing sig body env = do
 let newRef = allocRef env in createFunction name (Just newRef) sig body env

getProgramEnvironmentDefault :: ExecutionResult -> Environment -> Environment
getProgramEnvironmentDefault (Executed _ _ env) _ = env
getProgramEnvironmentDefault _ envDefault = envDefault

callOperator :: Ident -> Integer -> [RuntimeValue] -> Exec (RuntimeValue, Environment)
callOperator name@(Ident nameStr) priority args = do
  env <- ask
  (case getDef name env of
    DInvalid -> raise $ "Called operator do not exist: " ++ nameStr
    (DOperator opName opPrio opBody) ->
      if (opName == name && opPrio == priority) then
         callFunctionF (RfFun (RFunSig $ length args) opBody) args env
      else raise $ "Invalid operator called: " ++ nameStr
    _ -> raise $ "Could not call: "++ nameStr ++ " it's not an operator. ")

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

createOperatorFunction :: Ident -> Integer -> RFunSig -> RFunBody -> Exec (RuntimeValue, Environment)
createOperatorFunction name priority sig body = do
  defEnv <- ask
  (val, env1) <- return $ newFunction sig body defEnv
  (env2) <- return $ setDef name (DOperator name priority body) env1
  (env2) <- return $ setVariable name val env2
  return (val, env2)

createOperator :: OperatorAny -> RFunSig -> RFunBody -> Exec (RuntimeValue, Environment)
createOperator (OperatorAnyF (OperatorF name)) sig body = createOperatorFunction (Ident name) 5 sig body
createOperator (OperatorAnyE (OperatorE name)) sig body = createOperatorFunction (Ident name) 4 sig body
createOperator (OperatorAnyDS OperatorDS) sig body = createOperatorFunction (Ident "*") 3 sig body
createOperator (OperatorAnyD (OperatorD name)) sig body = createOperatorFunction (Ident name) 3 sig body
createOperator (OperatorAnyC (OperatorC name)) sig body = createOperatorFunction (Ident name) 2 sig body
createOperator (OperatorAnyB (OperatorB name)) sig body = createOperatorFunction (Ident name) 1 sig body
createOperator (OperatorAnyA (OperatorA name)) sig body = createOperatorFunction (Ident name) 0 sig body
