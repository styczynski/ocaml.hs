module Inference.Syntax where

import Syntax.Base

import Inference.Types
import Inference.TypingEnvironment

data SimplifiedExpr
  = SimplifiedVariable Ident
  | SimplifiedCall SimplifiedExpr SimplifiedExpr
  | SimplifiedFunction Ident SimplifiedExpr
  | SimplifiedLet Ident SimplifiedExpr SimplifiedExpr
  | SimplifiedConst Lit
  | SimplifiedIf SimplifiedExpr SimplifiedExpr SimplifiedExpr
  | SimplifiedFixPoint SimplifiedExpr
  | SimplifiedBinaryOp BinaryOp SimplifiedExpr SimplifiedExpr
  | SimplifiedUnaryOp UnaryOp SimplifiedExpr
  | SimplifiedSkip
  | SimplifiedCheck SimplifiedExpr Scheme
  | SimplifiedExportEnv
  | SimplifiedTyped Scheme
  | SimplifiedAnnotated String SimplifiedExpr
  deriving (Show, Eq)

data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Show, Eq)

data BinaryOp = OpSemicolon | OpSame | OpCustom String | OpCons | OpTupleCons
  deriving (Eq, Ord, Show)

data UnaryOp = OpCustomUni String | OpHead | OpTails | OpEmptyList | OpEmptyTuple | OpTupleNth Int Int | OpListNth
  deriving (Eq, Ord, Show)

data Program = Program [Decl] SimplifiedExpr deriving Eq

type Decl = (String, SimplifiedExpr)
