module Syntax where

import AbsSyntax
import Type
import Env

data Expr
  = Var Ident
  | App Expr Expr
  | Lam Ident Expr
  | Let Ident Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  | UniOp Uniop Expr
  | Skip
  | Check Expr Scheme
  | Export
  | Typed Scheme
  | Annot String Expr
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

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
