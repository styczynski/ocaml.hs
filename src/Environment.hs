module Environment where

import AbsSyntax
import qualified Data.Map as Map

data RuntimeValue = RUnit | RInt Integer | RString String deriving (Show, Eq)

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