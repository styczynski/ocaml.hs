module Inference.Syntax where

import AbsSyntax
import Inference.Types
import Inference.TypingEnvironment

data SimplifiedExpr
  = SVariable Ident
  | SCall SimplifiedExpr SimplifiedExpr
  | SFunction Ident SimplifiedExpr
  | SLet Ident SimplifiedExpr SimplifiedExpr
  | SConst Lit
  | SIf SimplifiedExpr SimplifiedExpr SimplifiedExpr
  | SFixPoint SimplifiedExpr
  | Op Binop SimplifiedExpr SimplifiedExpr
  | UniOp Uniop SimplifiedExpr
  | SSkip
  | SCheck SimplifiedExpr Scheme
  | SExportEnv
  | STyped Scheme
  | SAnnotated String SimplifiedExpr
  deriving (Show, Eq)

data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Show, Eq)

data Binop = OpSemicolon | OpSame | OpCustom String | OpCons | OpTupleCons
  deriving (Eq, Ord, Show)

data Uniop = OpCustomUni String | OpHead | OpTails | OpEmptyList | OpEmptyTuple | OpTupleNth Int Int | OpListNth
  deriving (Eq, Ord, Show)

data Program = Program [Decl] SimplifiedExpr deriving Eq

type Decl = (String, SimplifiedExpr)
