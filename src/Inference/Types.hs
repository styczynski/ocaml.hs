{-|
Module      : Inference.Types
Description : Types for inferencer
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides all base and complex types used by inferencer.
-}
module Inference.Types where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           Syntax.Base             hiding ( TV )

import qualified Data.Map                      as Map

-- | Free type variable
newtype TypeVar = TV String
  deriving (Show, Eq, Ord)

-- | Typing environment
data TypeEnvironment = TypeEnvironment { types :: Map.Map Ident Scheme }
  deriving (Eq, Show)

data TypeAnnotation = AnnotationEnv TypeEnvironment deriving (Show, Eq)

-- | Data types for inference
data Type
  = TypeVar TypeVar
  | TypeStatic String
  | TypeArrow Type Type
  | TypeList Type
  | TypeTuple Type Type
  | TypeUnit
  | TypeComplex String [Type]
  | TypeAnnotated TypeAnnotation
  deriving (Show, Eq)

-- | Type scheme
data Scheme = Scheme [TypeVar] Type
  deriving (Show, Eq)

-- | Extracts free variables from the type
getTypeFVNames :: Type -> [String]
getTypeFVNames (TypeVar  (TV name)) = [name]
getTypeFVNames (TypeList t        ) = getTypeFVNames t
getTypeFVNames (TypeArrow a b     ) = (getTypeFVNames a) ++ (getTypeFVNames b)
getTypeFVNames (TypeComplex name deps) =
  foldr (\t acc -> acc ++ (getTypeFVNames t)) [] deps
getTypeFVNames (TypeTuple a b) = (getTypeFVNames a) ++ (getTypeFVNames b)
getTypeFVNames _               = []

-- | Helper to reassign types for readability
remapTypesRec :: Map.Map String String -> Type -> Type
remapTypesRec fvMap t@(TypeVar (TV name)) = case Map.lookup name fvMap of
  (Just newName) -> (TypeVar (TV newName))
  (Nothing     ) -> t
remapTypesRec fvMap (TypeAnnotated _) = TypeUnit
remapTypesRec fvMap (TypeList      t) = TypeList $ remapTypesRec fvMap t
remapTypesRec fvMap (TypeArrow a b) =
  TypeArrow (remapTypesRec fvMap a) (remapTypesRec fvMap b)
remapTypesRec fvMap (TypeComplex name deps) =
  TypeComplex name $ map (remapTypesRec fvMap) deps
remapTypesRec fvMap (TypeTuple a b) =
  TypeTuple (remapTypesRec fvMap a) (remapTypesRec fvMap b)
remapTypesRec _ v = v

-- | Gets unique free variables names
fvUnique :: (Eq a) => [a] -> [a]
fvUnique []       = []
fvUnique (x : xs) = x : fvUnique (filter (/= x) xs)

-- | Generator for readable types (remapTypes)
typesLetters :: [String]
typesLetters = [1 ..] >>= flip replicateM ['a' .. 'z']

-- | Map types to fresh ones (for readability)
remapTypes :: Type -> Type
remapTypes t =
  let fvNames = fvUnique $ getTypeFVNames t
  in  let fvMap =
              foldr (\(l, name) acc -> Map.insert name ("'" ++ l) acc) Map.empty
                $ zip typesLetters fvNames
      in  remapTypesRec fvMap t

-- | Helper to print types in readable format
typeToStrRec :: [TypeVar] -> Type -> String
typeToStrRec vars TypeUnit = "()"
typeToStrRec vars (TypeAnnotated (AnnotationEnv v)) =
  "export{" ++ (show v) ++ "}"
typeToStrRec vars (TypeList t) = "[" ++ (typeToStrRec vars t) ++ "]"
typeToStrRec vars (TypeArrow a b) =
  "(" ++ (typeToStrRec vars a) ++ ") -> " ++ (typeToStrRec vars b)
typeToStrRec vars (TypeVar    (TV name)) = name
typeToStrRec vars (TypeStatic name     ) = name
typeToStrRec vars (TypeComplex name deps) =
  name
    ++ " ("
    ++ (foldr
         (\el acc ->
           acc
             ++ (if length acc <= 0 then "" else ", ")
             ++ (typeToStrRec vars el)
         )
         ""
         deps
       )
    ++ ")"
typeToStrRec vars (TypeTuple TypeUnit TypeUnit) = "()"
typeToStrRec vars (TypeTuple a (TypeTuple TypeUnit TypeUnit)) =
  typeToStrRec vars a
typeToStrRec vars (TypeTuple a b) =
  (typeToStrRec vars a) ++ " * " ++ (typeToStrRec vars b)

-- | Print readable text representation for type
typeToStr :: [TypeVar] -> Type -> String
typeToStr l t = typeToStrRec l $ remapTypes t

-- | Print readable text representation for type schema
schemeToStr :: Scheme -> String
schemeToStr (Scheme vars t) = typeToStr vars t

