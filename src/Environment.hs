module Environment where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map

import AbsSyntax

type Exec = StateT (Environment) (ReaderT (Environment) (ExceptT String IO))
type ProgramFn = Environment -> Exec RuntimeValue

data RFuncSignature = RFuncSignature [RuntimeType] RuntimeType
data RFuncBody = RFuncBody ([RuntimeValue] -> Environment -> Exec RuntimeValue)

instance Show RFuncBody where
  show (RFuncBody fn) = "<function-body>"

instance Show RFuncSignature where
  show sig = typeToString (TFunc sig)

data RuntimeType = TUnit | TInt | TString | TFunc RFuncSignature | TUnknown
getType :: RuntimeValue -> RuntimeType
getType RUnit = TUnit
getType (RInt _) = TInt
getType (RString _) = TString
getType (RFunc sig _) = TFunc sig
getType _ = TUnknown

typeToString :: RuntimeType -> String
typeToString TUnit = "unit"
typeToString TInt = "int"
typeToString TString = "string"
typeToString TUnknown = "unknown"
typeToString (TFunc (RFuncSignature params ret)) =
  "function: " ++ (foldl (\acc el -> (if (acc == "") then "" else (acc ++ "-> ")) ++ (typeToString el) ++ " ") "" (params ++ [ret]))

getTypeString :: RuntimeValue -> String
getTypeString val = typeToString $ getType $ val

data RuntimeValue = RUnit | RInt Integer | RString String | RFunc RFuncSignature RFuncBody deriving (Show)

data Environment = Environment { variables :: (Map.Map Ident RuntimeValue) } deriving (Show)

emptyEnv = Environment { variables=Map.empty }

setVariable :: Environment -> Ident -> RuntimeValue -> Environment
setVariable env name val =
  let Environment { variables = oldVariables } = env in
  env { variables = (Map.insert name val oldVariables) }

delVariable :: Environment -> Ident -> Environment
delVariable env name =
  let Environment { variables = oldVariables } = env in
  env { variables = (Map.delete name oldVariables) }

getVariable :: Environment -> Ident -> RuntimeValue
getVariable env name =
  let Environment { variables = oldVariables } = env in
  Map.findWithDefault RUnit name oldVariables