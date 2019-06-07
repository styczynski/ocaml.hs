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

data Environment = Environment {
  variables       :: (Map.Map Ident RuntimeValue),
  refs            :: (Map.Map Integer RuntimeRefValue),
  defs            :: (Map.Map Ident RuntimeDef),
  nextFreeRef     :: Integer
}

data InterpreterState = InterpreterState {
  trace           :: [String],
  lastNode        :: String,
  lastNodeDetail  :: String
}

data ExecutionResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment
type Exec = StateT (InterpreterState) (ReaderT (Environment) (ExceptT String IO))

type RFunBody = [RuntimeValue] -> Exec (RuntimeValue, Environment)
data RFunSig = RFunSig Int deriving (Show, Eq)

data RuntimeRefValue
  = RfFun RFunSig RFunBody
  | RfInvalid RuntimeValue

data RuntimeValue
  = REmpty
  | RInvalid
  | RInt Integer
  | RString String
  | RBool Bool
  | RVariant Ident Ident RuntimeValue
  | RTuple [RuntimeValue]
  | RList [RuntimeValue]
  | RRef Integer
  deriving (Show, Eq)

data RuntimeDef
  = DInvalid
  | DVariant Ident Ident
  deriving (Show, Eq)

data RuntimeType
  = TEmpty
  | TInvalid
  | TInt
  | TString
  | TBool
  | TTuple [RuntimeType]
  | TList RuntimeType
  | TListEmpty
  | TVariant Ident Ident RuntimeType
  | TFun RFunSig
  deriving (Show, Eq)

getType :: (RuntimeValue, Environment) -> RuntimeType
getType (REmpty, _) = TEmpty
getType ((RInt _), _) = TInt
getType ((RString _), _) = TString
getType ((RBool _), _) = TBool
getType (RInvalid, _) = TInvalid
getType ((RVariant name option val), env) = TVariant name option $ getType (val,env)
getType ((RTuple elems), env) = TTuple $ map (\e -> getType (e,env)) elems
getType ((RList []), _) = TListEmpty
getType ((RList (h:t)), env) = TList $ getType (h,env)
getType ((RRef id), env) =
  let Environment { refs = refs } = env in
    case Map.findWithDefault (RfInvalid RInvalid) id refs of
      RfInvalid _ -> TInvalid
      RfFun sig body -> TFun sig

typeToStr :: RuntimeType -> String
typeToStr TEmpty = "()"
typeToStr TInt = "int"
typeToStr TString = "string"
typeToStr TBool = "bool"
typeToStr TInvalid = "invalid"
typeToStr (TVariant (Ident name) _ _) = name
typeToStr (TTuple []) = "()"
typeToStr (TTuple elems) = (foldl (\acc el ->
   if (acc == "") then typeToStr el else acc ++ " * " ++ (typeToStr el)
 ) "" elems)
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

proceedD :: (Show a, Print a) => a -> Exec ()
proceedD a = do
  state <- get
  put $ let InterpreterState { trace = trace } = state in state { lastNodeDetail = (treeToStr a), trace = ([(treeToStr a)] ++ trace) }

proceed :: (Show a, Print a) => a -> Exec ()
proceed a = do
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
      let (fInd : _) = indices (T.pack lastNodeDetail) (T.pack lastNode) in
        let pointerText = (T.unpack (T.replicate fInd (T.pack " "))) ++ "^" in
          throwError $ " RuntimeError:\n" ++ "   " ++ lastNode ++ "\n   " ++ pointerText ++ "\n    " ++ errorText ++ "\n" ++ traceStr

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

vmapBool :: (Bool -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapBool fn e1 = do
  (val1, env) <- e1 >>= unpackBool
  return ((fn val1), env)

vmapBool2 :: (Bool -> Bool -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapBool2 fn e1 e2 = do
  (val1, env1) <- e1 >>= unpackBool
  (val2, env2) <- e2 >>= unpackBool
  return ((fn val1 val2), env2)

vmapInt :: (Integer -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapInt fn e1 = do
  (val1, env1) <- e1 >>= unpackInt
  return ((fn val1), env1)

vmapInt2 :: (Integer -> Integer -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapInt2 fn e1 e2 = do
  (val1, env1) <- e1 >>= unpackInt
  (val2, env2) <- e2 >>= unpackInt
  return ((fn val1 val2), env2)
