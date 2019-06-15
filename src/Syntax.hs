module Syntax where

import AbsSyntax

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
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql | OpCons | OpTupleCons
  deriving (Eq, Ord, Show)

data Uniop = OpHead | OpTails | OpEmptyList | OpEmptyTuple | OpTupleNth Int Int
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
