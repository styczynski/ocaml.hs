module Type where

import AbsSyntax
import qualified Data.Map as Map

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Env = TypeEnv { types :: Map.Map Ident Scheme }
  deriving (Eq, Show)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  | TList Type
  | TTuple Type Type
  | TUnit
  | TExport Env
  | TDep String [Type]
  deriving (Show, Eq)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq)

typeInt, typeBool, typeString :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"
typeString = TCon "String"

typeToStr :: [TVar] -> Type -> String
typeToStr vars TUnit = "()"
typeToStr vars (TExport v) = "export{" ++ (show v) ++"}"
typeToStr vars (TList t) = "[" ++ (typeToStr vars t) ++ "]"
typeToStr vars (TArr a b) = "(" ++ (typeToStr vars a) ++ ") -> " ++ (typeToStr vars b)
typeToStr vars (TVar (TV name)) = name ++ "'"
typeToStr vars (TCon name) = name
typeToStr vars (TDep name deps) = name ++ " (" ++ (foldr (\el acc -> acc ++ (if length acc <= 0 then "" else ", ") ++ (typeToStr vars el)) "" deps) ++ ")"
typeToStr vars (TTuple TUnit TUnit) = "()"
typeToStr vars (TTuple a (TTuple TUnit TUnit)) = typeToStr vars a
typeToStr vars (TTuple a b) = (typeToStr vars a) ++ " * " ++ (typeToStr vars b)

schemeToStr :: Scheme -> String
schemeToStr (Forall vars t) = typeToStr vars t

