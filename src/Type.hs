module Type where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

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

getTypeFVNames :: Type -> [String]
getTypeFVNames (TVar (TV name)) = [name]
getTypeFVNames (TList t) = getTypeFVNames t
getTypeFVNames (TArr a b) = (getTypeFVNames a) ++ (getTypeFVNames b)
getTypeFVNames (TDep name deps) = foldr (\t acc -> acc ++ (getTypeFVNames t)) [] deps
getTypeFVNames (TTuple a b) = (getTypeFVNames a) ++ (getTypeFVNames b)
getTypeFVNames _ = []

remapTypesRec :: Map.Map String String -> Type -> Type
remapTypesRec fvMap t@(TVar (TV name)) =
  case Map.lookup name fvMap of
    (Just newName) -> (TVar (TV newName))
    (Nothing) -> t
remapTypesRec fvMap (TExport _) = TUnit
remapTypesRec fvMap (TList t) = TList $ remapTypesRec fvMap t
remapTypesRec fvMap (TArr a b) = TArr (remapTypesRec fvMap a) (remapTypesRec fvMap b)
remapTypesRec fvMap (TDep name deps) = TDep name $ map (remapTypesRec fvMap) deps
remapTypesRec fvMap (TTuple a b) = TTuple (remapTypesRec fvMap a) (remapTypesRec fvMap b)
remapTypesRec _ v = v

fvUnique :: (Eq a) => [a] -> [a]
fvUnique [] = []
fvUnique (x:xs) = x : fvUnique (filter (/=x) xs)

typesLetters :: [String]
typesLetters = [1..] >>= flip replicateM ['a'..'z']

remapTypes :: Type -> Type
remapTypes t =
  let fvNames = fvUnique $ getTypeFVNames t in
  let fvMap = foldr (\(l, name) acc -> Map.insert name ("'" ++ l) acc) Map.empty $ zip typesLetters fvNames in
  remapTypesRec fvMap t

typeToStrRec :: [TVar] -> Type -> String
typeToStrRec vars TUnit = "()"
typeToStrRec vars (TExport v) = "export{" ++ (show v) ++"}"
typeToStrRec vars (TList t) = "[" ++ (typeToStrRec vars t) ++ "]"
typeToStrRec vars (TArr a b) = "(" ++ (typeToStrRec vars a) ++ ") -> " ++ (typeToStrRec vars b)
typeToStrRec vars (TVar (TV name)) = name
typeToStrRec vars (TCon name) = name
typeToStrRec vars (TDep name deps) = name ++ " (" ++ (foldr (\el acc -> acc ++ (if length acc <= 0 then "" else ", ") ++ (typeToStrRec vars el)) "" deps) ++ ")"
typeToStrRec vars (TTuple TUnit TUnit) = "()"
typeToStrRec vars (TTuple a (TTuple TUnit TUnit)) = typeToStrRec vars a
typeToStrRec vars (TTuple a b) = (typeToStrRec vars a) ++ " * " ++ (typeToStrRec vars b)

typeToStr :: [TVar] -> Type -> String
typeToStr l t = typeToStrRec l $ remapTypes t

schemeToStr :: Scheme -> String
schemeToStr (Forall vars t) = typeToStr vars t

