{-# LANGUAGE FlexibleInstances #-}

module Runtime where

import AbsSyntax
import PrintSyntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Text.Internal.Search
import qualified Data.Text as T
import qualified Data.Map as Map

import System.IO
import System.IO.Unsafe

data Environment = Environment {
  variables       :: (Map.Map Ident RuntimeValue),
  refs            :: (Map.Map Integer RuntimeRefValue),
  defs            :: (Map.Map Ident RuntimeDef),
  nextFreeRef     :: Integer
}

data InterpreterState = InterpreterState {
  trace           :: [String],
  lastNode        :: String,
  lastNodeDetail  :: String,
  globalExportEnv :: (Maybe Environment)
}

data ExecutionResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment
type Exec = StateT (InterpreterState) (ReaderT (Environment) (ExceptT String IO))

type RFunBody = [RuntimeValue] -> Exec (RuntimeValue, Environment)
data RFunSig = RFunSig Int deriving (Show, Eq)

data RuntimeRefValue
  = RfFun RFunSig RFunBody
  | RfVal RuntimeValue
  | RfInvalid RuntimeValue

data RuntimeValue
  = REmpty
  | RInvalid
  | RInt Integer
  | RString String
  | RBool Bool
  | RVariant Ident Ident RuntimeValue
  | RRecord Ident (Map.Map Ident RuntimeValue)
  | RTuple [RuntimeValue]
  | RList [RuntimeValue]
  | RRef Integer
  deriving (Show, Eq)

data RuntimeDef
  = DInvalid
  | DVariant Ident Ident
  | DRecord Ident Ident
  | DOperator Ident Integer RFunBody

data RuntimeType
  = TEmpty
  | TVar String
  | TInvalid
  | TInt
  | TRef RuntimeType
  | TString
  | TBool
  | TTuple [RuntimeType]
  | TList RuntimeType
  | TListEmpty
  | TVariant Ident Ident RuntimeType
  | TRecord Ident
  | TFun RFunSig
  | TFunEx RuntimeType RuntimeType
  deriving (Show, Eq)

class PackableValue a where
  packVal :: a -> Exec (RuntimeValue, Environment)

class UnpackableValue a where
  unpackVal :: RuntimeValue -> Exec (a, Environment)

instance PackableValue Integer where
  packVal v = do
    env <- ask
    return ((RInt v), env)

instance UnpackableValue Integer where
  unpackVal (RInt v) = do
    env <- ask
    return (v, env)

instance PackableValue String where
  packVal v = do
    env <- ask
    return ((RString v), env)

instance UnpackableValue String where
  unpackVal (RString v) = do
    env <- ask
    return (v, env)

instance PackableValue Bool where
  packVal v = do
    env <- ask
    return ((RBool v), env)

instance UnpackableValue Bool where
  unpackVal (RBool v) = do
    env <- ask
    return (v, env)

getType :: (RuntimeValue, Environment) -> RuntimeType
getType (REmpty, _) = TEmpty
getType ((RInt _), _) = TInt
getType ((RString _), _) = TString
getType ((RBool _), _) = TBool
getType (RInvalid, _) = TInvalid
getType ((RRecord name _), env) = TRecord name
getType ((RVariant name option val), env) = TVariant name option $ getType (val,env)
getType ((RTuple elems), env) = TTuple $ map (\e -> getType (e,env)) elems
getType ((RList []), _) = TListEmpty
getType ((RList (h:t)), env) = TList $ getType (h,env)
getType ((RRef id), env) =
  let Environment { refs = refs } = env in
    case Map.findWithDefault (RfInvalid RInvalid) id refs of
      RfInvalid _ -> TInvalid
      RfFun sig body -> TFun sig
      RfVal val -> TRef $ getType (val, env)

typeToStr :: RuntimeType -> String
typeToStr (TVar name) = name ++ "'"
typeToStr TEmpty = "()"
typeToStr TInt = "int"
typeToStr TString = "string"
typeToStr TBool = "bool"
typeToStr TInvalid = "invalid"
typeToStr (TRef v) = "ref " ++ (typeToStr v)
typeToStr (TRecord (Ident name)) = name
typeToStr (TVariant (Ident name) _ _) = name
typeToStr (TTuple []) = "()"
typeToStr (TTuple elems) = (foldl (\acc el ->
   if (acc == "") then typeToStr el else acc ++ " * " ++ (typeToStr el)
 ) "" elems)
typeToStr (TFunEx a b) = (typeToStr a) ++ " -> " ++ (typeToStr b)
typeToStr TListEmpty = "[]"
typeToStr (TList instype) = "[" ++ (typeToStr instype) ++ "]"
typeToStr (TFun (RFunSig argsCount)) = "function<" ++ (show argsCount) ++ ">"

getTypeStr :: (RuntimeValue, Environment) -> String
getTypeStr = typeToStr . getType

instance Show RuntimeRefValue where
  show (RfInvalid val) = "<Invalid " ++ (show val) ++ ">"
  show (RfFun _ _) = "<Function>"

treeToStr :: (Show a, Print a) => a -> String
treeToStr tree = printTree tree

runtimePrint :: String -> Exec ()
runtimePrint str = do
  lift $ lift $ lift $ putStrLn str

envToStr :: Environment -> String
envToStr env =
  let Environment { variables = variables } = env in
    " { " ++ (Map.foldlWithKey (\acc (Ident name) val ->
       if (acc == "") then name ++ "=" ++ (valueToStr val) else acc ++ "\n " ++ name ++ "=" ++ (valueToStr val)
     ) "" variables) ++ " }"

debug ast = do
  env <- ask
  runtimePrint $ "Code: " ++ (treeToStr ast) ++ ", env: " ++ (envToStr env)

proceedD :: (Show a, Print a) => a -> Exec ()
proceedD a = do
  --debug a
  state <- get
  put $ let InterpreterState { trace = trace } = state in state { lastNodeDetail = (treeToStr a), trace = ([(treeToStr a)] ++ trace) }

proceed :: (Show a, Print a) => a -> Exec ()
proceed a = do
  --debug a
  state <- get
  put $ let InterpreterState { trace = trace } = state in state { lastNode = (treeToStr a), lastNodeDetail = (treeToStr a), trace = ([(treeToStr a)])  }

proceedT :: (Show a, Print a) => a -> (Exec (RuntimeValue, Environment)) -> (Exec (RuntimeValue, Environment))
proceedT a val = do
  proceed a
  val

raise :: String -> Exec a
raise errorText = do
  state <- get
  let InterpreterState { lastNode = lastNode, lastNodeDetail = lastNodeDetail, trace = trace } = state in
    let traceStr = foldl (\acc el -> acc ++ "     | Execute: " ++ el ++ "\n") "" trace in
      case indices (T.pack lastNodeDetail) (T.pack lastNode) of
        (fInd : _) -> let pointerText = (T.unpack (T.replicate fInd (T.pack " "))) ++ "^" in
          throwError $ " RuntimeError:\n" ++ "   " ++ lastNode ++ "\n   " ++ pointerText ++ "\n    " ++ errorText ++ "\n" ++ traceStr
        _ -> throwError $ " RuntimeError:\n" ++ "   " ++ lastNode ++ "\n    " ++ errorText ++ "\n" ++ traceStr

resultToStr :: ExecutionResult -> String
resultToStr (Executed val _) = valueToStr val
resultToStr (FailedParse err) = err
resultToStr (FailedExecution err) = err

valueToStr :: RuntimeValue -> String
valueToStr REmpty = "()"
valueToStr (RInt val) = show val
valueToStr (RString val) = show val
valueToStr (RVariant _ (Ident option) val) = option ++ " " ++ (valueToStr val)
valueToStr (RBool val) = show val
valueToStr (RTuple []) = "()"
valueToStr (RRecord _ fields) = " { " ++ (Map.foldlWithKey (\acc (Ident fieldName) fieldVal ->
    if (acc == "") then fieldName ++ "=" ++ (valueToStr fieldVal) else acc ++ "; " ++ fieldName ++ "=" ++ (valueToStr fieldVal)
  ) "" fields) ++ " }"
valueToStr (RTuple vals) = "( " ++ (foldl (\acc el ->
    if (acc == "") then valueToStr el else acc ++ ", " ++ (valueToStr el)
  ) "" vals) ++ " )"
valueToStr (RList vals) = "[ " ++ (foldl (\acc el ->
    if (acc == "") then valueToStr el else acc ++ "; " ++ (valueToStr el)
  ) "" vals) ++ " ]"
valueToStr (RRef _) = "<ref>"
valueToStr (RInvalid) = "Invalid"

unpackBool :: (RuntimeValue, Environment) -> Exec (Bool, Environment)
unpackBool arg@(val, env) =
  case val of
    RBool v -> return (v, env)
    _ -> raise $ "Expected type: " ++ (typeToStr TBool) ++ ", got: " ++ (getTypeStr arg)

unpackInt :: (RuntimeValue, Environment) -> Exec (Integer, Environment)
unpackInt arg@(val, env) =
  case val of
    RInt v -> return (v, env)
    _ -> raise $ "Expected type: " ++ (typeToStr TInt) ++ ", got: " ++ (getTypeStr arg)

unpackString :: (RuntimeValue, Environment) -> Exec (String, Environment)
unpackString arg@(val, env) =
  case val of
    RString v -> return (v, env)
    _ -> raise $ "Expected type: " ++ (typeToStr TString) ++ ", got: " ++ (getTypeStr arg)

--vmapBool :: (Bool -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
--vmapBool fn e1 = do
--  (val1, env) <- e1 >>= unpackBool
--  return ((fn val1), env)
--
--vmapBool2 :: (Bool -> Bool -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
--vmapBool2 fn e1 e2 = do
--  (val1, env1) <- e1 >>= unpackBool
--  (val2, env2) <- e2 >>= unpackBool
--  return ((fn val1 val2), env2)
--
--vmapInt :: (Integer -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
--vmapInt fn e1 = do
--  (val1, env1) <- e1 >>= unpackInt
--  return ((fn val1), env1)
--
--vmapInt2 :: (Integer -> Integer -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
--vmapInt2 fn e1 e2 = do
--  (val1, env1) <- e1 >>= unpackInt
--  (val2, env2) <- e2 >>= unpackInt
--  return ((fn val1 val2), env2)
