{-|
Module      : Inference.Syntax
Description : Simplified AST structures
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This code introduces simplified AST structure.

  The simplified AST is produced from normal program AST by Inference.Simplifier.
  It allows only very basic constructions in addition to
  type assertions, dummy values (that do not exist, but have their exact type) and more
  constructions that make type inference easier to implement.
-}
module Inference.Syntax where

import           Syntax.Base

import           Inference.Types
import           Inference.TypingEnvironment

data SimplifiedExpr
  = SimplifiedVariable Ident
  | SimplifiedCall SimplifiedExpr SimplifiedExpr
  | SimplifiedFunction Ident SimplifiedExpr
  | SimplifiedLet Ident SimplifiedExpr SimplifiedExpr
  | SimplifiedIf SimplifiedExpr SimplifiedExpr SimplifiedExpr
  | SimplifiedFixPoint SimplifiedExpr
  | SimplifiedBinaryOp BinaryOp SimplifiedExpr SimplifiedExpr
  | SimplifiedUnaryOp UnaryOp SimplifiedExpr
  | SimplifiedSkip
  | SimplifiedCheck SimplifiedExpr Scheme
  | SimplifiedExportEnv
  | SimplifiedTyped Scheme
  | SimplifiedAnnotated String SimplifiedExpr
  | SimplifiedConstBool Bool
  | SimplifiedConstInt Integer
  | SimplifiedConstString String
  deriving (Show, Eq)

data BinaryOp = OpSemicolon | OpSame | OpCustom String | OpCons | OpTupleCons
  deriving (Eq, Ord, Show)

data UnaryOp = OpCustomUni String | OpHead | OpTails | OpEmptyList | OpEmptyTuple | OpTupleNth Int Int | OpListNth
  deriving (Eq, Ord, Show)

data Program = Program [Decl] SimplifiedExpr deriving Eq

type Decl = (String, SimplifiedExpr)
