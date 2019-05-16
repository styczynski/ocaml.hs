module Environment where

import AbsSyntax
import qualified Data.Map as Map

data RuntimeValue = RUnit | RInt Integer | RString String deriving (Show, Eq)

data Environment = Environment { variables :: (Map.Map Ident RuntimeValue) } deriving (Show)

emptyEnv = Environment { variables=Map.empty }