{-|
Module      : Runtime.Runtime
Description : base runtime definitions
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module exports base definitions for runtime operations like
  variable containers, environment and state definitions etc.
-}
{-# LANGUAGE FlexibleInstances #-}

module Runtime.Runtime where

import           Syntax.Base

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Text.Internal.Search
import qualified Data.Text                     as T
import qualified Data.Map                      as Map

import           Inference.TypingEnvironment
import           Inference.Inferencer
import           Inference.Errors
import qualified Inference.Types               as Types

import           System.IO
import           System.IO.Unsafe

data Environment = Environment {
  variables       :: (Map.Map Ident RuntimeValue),
  refs            :: (Map.Map Integer RuntimeRefValue),
  defs            :: (Map.Map Ident RuntimeDef),
  nextFreeRef     :: Integer,
  nextFreeNameId  :: Integer,
  itypes           :: Types.Env,
  stypes           :: InferState
}

data InterpreterState = InterpreterState {
  trace           :: [String],
  lastNode        :: String,
  lastNodeDetail  :: String,
  globalExportEnv :: (Maybe Environment)
}

data ExecutionResult = FailedParse String | FailedExecution String | Executed RuntimeValue Types.Type Environment | FailedTypechecking TypeError
type Exec
  = StateT (InterpreterState) (ReaderT (Environment) (ExceptT String IO))

type RFunBody = [RuntimeValue] -> Exec (RuntimeValue, Environment)
data RFunSig = RFunVararg | RFunSig Int deriving (Show, Eq)

data RuntimeRefValue
  = RfFun RFunSig RFunBody
  | RfVal RuntimeValue
  | RfInvalid RuntimeValue

instance Eq Environment where
  a == b = False

instance Show Environment where
  show a = "<env>"

data RuntimeValue
  = REmpty
  | RExport Environment
  | RInvalid
  | RInt Integer
  | RString String
  | RBool Bool
  | RVariant Ident Ident RuntimeValue
  | RRecord Ident (Map.Map Ident RuntimeValue)
  | RTuple [RuntimeValue]
  | RList [RuntimeValue]
  | RRef Integer
  | RInternalDupIdent
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
getType (REmpty          , _  ) = TEmpty
getType ((RExport _)     , _  ) = TEmpty
getType ((RInt    _)     , _  ) = TInt
getType ((RString _)     , _  ) = TString
getType ((RBool   _)     , _  ) = TBool
getType (RInvalid        , _  ) = TInvalid
getType ((RRecord name _), env) = TRecord name
getType ((RVariant name option val), env) =
  TVariant name option $ getType (val, env)
getType ((RTuple elems  ), env) = TTuple $ map (\e -> getType (e, env)) elems
getType ((RList  []     ), _  ) = TListEmpty
getType ((RList  (h : t)), env) = TList $ getType (h, env)
getType ((RRef id), env) =
  let Environment { refs = refs } = env
  in  case Map.findWithDefault (RfInvalid RInvalid) id refs of
        RfInvalid _    -> TInvalid
        RfFun sig body -> TFun sig
        RfVal val      -> TRef $ getType (val, env)

typeToStr :: RuntimeType -> String
typeToStr (TVar name)                 = name ++ "'"
typeToStr TEmpty                      = "()"
typeToStr TInt                        = "int"
typeToStr TString                     = "string"
typeToStr TBool                       = "bool"
typeToStr TInvalid                    = "invalid"
typeToStr (TRef    v                ) = "ref " ++ (typeToStr v)
typeToStr (TRecord (Ident name)     ) = name
typeToStr (TVariant (Ident name) _ _) = name
typeToStr (TTuple []                ) = "()"
typeToStr (TTuple elems) =
  (foldl
    (\acc el ->
      if (acc == "") then typeToStr el else acc ++ " * " ++ (typeToStr el)
    )
    ""
    elems
  )
typeToStr (TFunEx a b)                = (typeToStr a) ++ " -> " ++ (typeToStr b)
typeToStr TListEmpty                  = "[]"
typeToStr (TList instype            ) = "[" ++ (typeToStr instype) ++ "]"
typeToStr (TFun  (RFunSig argsCount)) = "function<" ++ (show argsCount) ++ ">"
typeToStr (TFun  (RFunVararg       )) = "function<...>"


getTypeStr :: (RuntimeValue, Environment) -> String
getTypeStr = typeToStr . getType

instance Show RuntimeRefValue where
  show (RfInvalid val) = "<Invalid " ++ (show val) ++ ">"
  show (RfFun _ _    ) = "<Function>"

treeToStr :: (Show a, Print a) => a -> String
treeToStr tree = printTree tree

runtimePrint :: String -> Exec ()
runtimePrint str = do
  lift $ lift $ lift $ putStrLn str

refToStr :: RuntimeRefValue -> String
refToStr (RfFun _ _  ) = "<function>"
refToStr (RfInvalid v) = "<invalid>"
refToStr (RfVal     v) = "<value>"

envToStr :: Environment -> String
--envToStr env =
--  let Environment { variables = variables } = env in
--    " { " ++ (Map.foldlWithKey (\acc (Ident name) val ->
--       if (acc == "") then name ++ "=" ++ (valueToStr val) else acc ++ "\n " ++ name ++ "=" ++ (valueToStr val)
--     ) "" variables) ++ " }"
envToStr env =
  let Environment { refs = refs, variables = variables } = env
  in  let strRefs =
              (  " { "
              ++ (Map.foldlWithKey
                   (\acc id val -> if (acc == "")
                     then (show id) ++ "=" ++ (refToStr val)
                     else acc ++ "\n " ++ (show id) ++ "=" ++ (refToStr val)
                   )
                   ""
                   refs
                 )
              ++ " }"
              )
      in  let strVals =
                  (  " { "
                  ++ (Map.foldlWithKey
                       (\acc (Ident name) val -> if (acc == "")
                         then name ++ "=" ++ (valueToStr env val)
                         else
                           acc ++ "\n " ++ name ++ "=" ++ (valueToStr env val)
                       )
                       ""
                       variables
                     )
                  ++ " }"
                  )
          in  strRefs ++ strVals

debug ast = do
  env <- ask
  runtimePrint $ "Code: " ++ (treeToStr ast) ++ ", env: " ++ (envToStr env)

--proceedD :: (Show a, Print a) => a -> Exec ()
--proceedD a = do
--  --debug a
--  state <- get
--  put $ let InterpreterState { trace = trace } = state in state { lastNodeDetail = (treeToStr a), trace = ([(treeToStr a)] ++ trace) }

proceed :: (Show a, Print a) => a -> Exec ()
proceed a = do
  --debug a
  state <- get
  put
    $ let InterpreterState { trace = trace } = state
      in  state { lastNode       = (treeToStr a)
                , lastNodeDetail = (treeToStr a)
                , trace          = ([(treeToStr a)] ++ trace)
                }

unproceed :: Exec ()
unproceed = do
  state <- get
  put
    $ let InterpreterState { trace = trace } = state
      in  state { trace = (drop 1 trace) }

--proceedT :: (Show a, Print a) => a -> (Exec (RuntimeValue, Environment)) -> (Exec (RuntimeValue, Environment))
--proceedT a val = do
--  proceed a
--  val

raise :: String -> Exec a
raise errorText = do
  (InterpreterState { lastNode = lastNode, lastNodeDetail = lastNodeDetail, trace = trace }) <-
    get
  traceStr <- return
    $ foldl (\acc el -> acc ++ "     | Execute: " ++ el ++ "\n") "" trace
  case indices (T.pack lastNodeDetail) (T.pack lastNode) of
    (fInd : _) ->
      let pointerText = (T.unpack (T.replicate fInd (T.pack " "))) ++ "^"
      in  throwError
            $  " RuntimeError:\n"
            ++ "   "
            ++ lastNode
            ++ "\n   "
            ++ pointerText
            ++ "\n    "
            ++ errorText
            ++ "\n"
            ++ traceStr
    _ ->
      throwError
        $  " RuntimeError:\n"
        ++ "   "
        ++ lastNode
        ++ "\n    "
        ++ errorText
        ++ "\n"
        ++ traceStr

resultToStr :: ExecutionResult -> String
resultToStr (Executed val _ env    ) = valueToStr env val
resultToStr (FailedParse        err) = err
resultToStr (FailedExecution    err) = err
resultToStr (FailedTypechecking err) = show err

valueToStrRec :: (Maybe Environment) -> RuntimeValue -> String
valueToStrRec env REmpty        = "()"
valueToStrRec env (RExport _  ) = "%export"
valueToStrRec env (RInt    val) = show val
valueToStrRec env (RString val) = show val
valueToStrRec env (RVariant _ (Ident option) val) =
  option ++ " " ++ (valueToStrRec env val)
valueToStrRec env (RBool  val) = show val
valueToStrRec env (RTuple [] ) = "()"
valueToStrRec env (RRecord _ fields) =
  "{ "
    ++ (Map.foldlWithKey
         (\acc (Ident fieldName) fieldVal -> if (acc == "")
           then fieldName ++ "=" ++ (valueToStrRec env fieldVal)
           else acc ++ "; " ++ fieldName ++ "=" ++ (valueToStrRec env fieldVal)
         )
         ""
         fields
       )
    ++ "}"
valueToStrRec env (RTuple vals) =
  "("
    ++ (foldl
         (\acc el -> if (acc == "")
           then valueToStrRec env el
           else acc ++ ", " ++ (valueToStrRec env el)
         )
         ""
         vals
       )
    ++ ")"
valueToStrRec env (RList vals) =
  "["
    ++ (foldl
         (\acc el -> if (acc == "")
           then valueToStrRec env el
           else acc ++ "; " ++ (valueToStrRec env el)
         )
         ""
         vals
       )
    ++ "]"
valueToStrRec Nothing (RRef fr) = "<ref: " ++ (show fr) ++ ">"
valueToStrRec (Just env) (RRef fr) =
  let Environment { refs = oldRefs } = env
  in  let r = Map.findWithDefault (RfInvalid RInvalid) fr oldRefs
      in  case r of
            (RfInvalid _) -> "<dangling_ref>"
            (RfFun _ _  ) -> "<function>"
            (RfVal v    ) -> valueToStrRec (Just env) v


valueToStrRec env (RInvalid) = "Invalid"

valueToStr :: Environment -> RuntimeValue -> String
valueToStr e r = valueToStrRec (Just e) r

valueToStrN :: RuntimeValue -> String
valueToStrN r = valueToStrRec Nothing r

unpackBool :: (RuntimeValue, Environment) -> Exec (Bool, Environment)
unpackBool arg@(val, env) = case val of
  RBool v -> return (v, env)
  _ ->
    raise
      $  "Expected type: "
      ++ (typeToStr TBool)
      ++ ", got: "
      ++ (getTypeStr arg)

unpackInt :: (RuntimeValue, Environment) -> Exec (Integer, Environment)
unpackInt arg@(val, env) = case val of
  RInt v -> return (v, env)
  _ ->
    raise
      $  "Expected type: "
      ++ (typeToStr TInt)
      ++ ", got: "
      ++ (getTypeStr arg)

unpackString :: (RuntimeValue, Environment) -> Exec (String, Environment)
unpackString arg@(val, env) = case val of
  RString v -> return (v, env)
  _ ->
    raise
      $  "Expected type: "
      ++ (typeToStr TString)
      ++ ", got: "
      ++ (getTypeStr arg)
