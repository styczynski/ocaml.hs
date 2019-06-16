module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  | TList Type
  | TTuple Type Type
  | TUnit
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"

typeToStr :: [TVar] -> Type -> String
typeToStr vars (TList t) = "[" ++ (typeToStr vars t) ++ "]"
typeToStr vars (TArr a b) = (typeToStr vars a) ++ " -> " ++ (typeToStr vars b)
typeToStr vars (TVar (TV name)) = name ++ "'"
typeToStr vars (TCon name) = name
typeToStr vars (TTuple TUnit TUnit) = "()"
typeToStr vars (TTuple a (TTuple TUnit TUnit)) = typeToStr vars a
typeToStr vars (TTuple a b) = (typeToStr vars a) ++ " * " ++ (typeToStr vars b)

schemeToStr :: Scheme -> String
schemeToStr (Forall vars t) = typeToStr vars t
