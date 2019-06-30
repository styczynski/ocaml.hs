module Inference.Simplifier where

import Syntax.Base

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Types
import Inference.Substitutions
import Inference.Errors
import Inference.InferencerUtils

import qualified Data.Map as Map
import qualified Data.Set as Set

type InferenceFn = TypeExpression -> Infer Scheme

getOperatorName :: OperatorAny -> String
getOperatorName (OperatorAnyA (OperatorA name)) = name
getOperatorName (OperatorAnyB (OperatorB name)) = name
getOperatorName (OperatorAnyC (OperatorC name)) = name
getOperatorName (OperatorAnyD (OperatorD name)) = name
getOperatorName (OperatorAnyDS (OperatorDS)) = "*"
getOperatorName (OperatorAnyE (OperatorE name)) = name
getOperatorName (OperatorAnyF (OperatorF name)) = name

simplifyPattern :: InferenceFn -> Bool -> SimplePattern -> Expr -> Expr -> Infer Expr
simplifyPattern _ _ PatNone _ expr = addExprAnnot $ return expr
simplifyPattern fn _ ast@(PatList (PList [])) letExpr expr = do
  markTrace ast
  scheme <- fn (TypeExprSimple $ TypeSExprList $ TypeExprSimple $ TypeSExprAbstract $ TypeIdentAbstract "'a")
  id <- freshIdent
  unmarkTrace ast
  addExprAnnot $ return $ Let id (Check letExpr scheme) expr
simplifyPattern fn recMode (PatConstr (Ident nameStr) pat) letExpr expr = do
  addExprAnnot $ simplifyPattern fn recMode pat (App (Var $ Ident $ nameStr ++ "_reverse") letExpr) expr
simplifyPattern fn recMode ast@(PatList (PList list)) letExpr expr = do
  markTrace ast
  (simpl, _) <- foldrM (\(PListElement pat) (expr, n) -> do
    patSimpl <- simplifyPattern fn recMode pat (UniOp OpListNth letExpr) expr
    return (patSimpl, n+1)) (expr, 0) list
  unmarkTrace ast
  addExprAnnot $ return simpl
simplifyPattern _ _ (PatConst const) letExpr expr = do
  id <- freshIdent
  addExprAnnot $ return $ Let id (Check letExpr $ getConstScheme const) expr
simplifyPattern fn recMode (PatCheck name typeExpr) letExpr expr = do
  scheme <- fn typeExpr
  addExprAnnot $ simplifyPattern fn recMode (PatIdent name) (Check letExpr scheme) expr
simplifyPattern _ False (PatIdent name) letExpr expr =
  addExprAnnot $ return $ Let name letExpr expr
simplifyPattern _ True (PatIdent name) letExpr expr =
  addExprAnnot $ return $ Let name (Fix (Lam name letExpr)) expr
simplifyPattern fn recMode ast@(PatCons hPat tPat) letExpr expr = do
  markTrace ast
  hSimpl <- simplifyPattern fn recMode hPat (UniOp OpHead letExpr) expr
  tSimpl <- simplifyPattern fn recMode tPat (UniOp OpTails letExpr) hSimpl
  unmarkTrace ast
  addExprAnnot $ return tSimpl
simplifyPattern fn recMode ast@(PatTuple (PTuple el restEls)) letExpr expr = do
  markTrace ast
  tupleCount <- return $ 1 + length restEls
  (tSimpl, _) <- foldrM (\(PTupleElement el) (expr, k) -> do
    pSimpl <- simplifyPattern fn recMode el (UniOp (OpTupleNth k tupleCount) letExpr) expr
    return (pSimpl, k+1)) (expr, 0) ([el] ++ restEls)
  unmarkTrace ast
  addExprAnnot $ return tSimpl

simplifyComplexExpression :: InferenceFn -> ComplexExpression -> Infer Expr
simplifyComplexExpression fn (ECTyped typeExpr) = do
  scheme <- fn typeExpr
  addExprAnnot $ return $ Typed scheme
simplifyComplexExpression fn ast@(ECFor varName initExpr dir endExpr bodyExpr) = do
  markTrace ast
  forCheckType <- return $ Forall [] $ (TCon "Int")
  initSimpl <- simplifyComplexExpression fn initExpr
  endSimpl <- simplifyComplexExpression fn endExpr
  initSimplCheck <- return $ Check initSimpl forCheckType
  endSimplCheck <- return $ Check endSimpl forCheckType
  bodySimpl <- simplifyComplexExpression fn bodyExpr
  unmarkTrace ast
  addExprAnnot $ return $ Let varName (Op OpSame initSimplCheck endSimplCheck) bodySimpl
simplifyComplexExpression fn ast@(ECWhile condExpr bodyExpr) = do
  markTrace ast
  whileCheckType <- return $ Forall [] $ (TCon "Bool")
  condSimpl <- simplifyComplexExpression fn condExpr
  bodySimpl <- simplifyComplexExpression fn bodyExpr
  condSimplCheck <- return $ Check condSimpl whileCheckType
  id <- freshIdent
  unmarkTrace ast
  addExprAnnot $ return $ Let id condSimplCheck bodySimpl
simplifyComplexExpression _ ECExportEnv = addExprAnnot $ return Export
simplifyComplexExpression fn (ECTuple tuple) = addExprAnnot $ simplifyTuple fn tuple
simplifyComplexExpression fn (ECExpr expr) = addExprAnnot $ simplifyExpression fn expr
simplifyComplexExpression fn ast@(ECIf cond exprThen exprElse) = do
  markTrace ast
  condSimpl <- simplifyComplexExpression fn cond
  thenSimpl <- simplifyComplexExpression fn exprThen
  elseSimpl <- simplifyComplexExpression fn exprElse
  unmarkTrace ast
  addExprAnnot $ return $ If condSimpl thenSimpl elseSimpl
simplifyComplexExpression fn ast@(ECMatch expr _ clauses) = do
  markTrace ast
  clausesList <- foldrM (\(MatchClause pat clauseExpr) acc -> do
    r <- return $ ListElement $ ECLet LetRecNo pat [] TypeConstrEmpty expr clauseExpr
    return $ [r] ++ acc) [] clauses
  r <- simplifyComplexExpression fn $ ECExpr $ ExprList $ DList clausesList
  unmarkTrace ast
  addExprAnnot $ return $ UniOp OpHead r
simplifyComplexExpression fn ast@(ECFunction bPip matchClauses) = do
  id <- freshIdent
  markTrace ast
  r <- simplifyComplexExpression fn $ ECFun (PatIdent id) [] $ ECMatch (ECExpr $ ExprVar id) bPip matchClauses
  unmarkTrace ast
  addExprAnnot $ return r
simplifyComplexExpression fn ast@(ECFun pat argsPat expr) = do
  id <- freshIdent
  markTrace ast
  r <- simplifyComplexExpression fn (ECLet LetRecNo (PatIdent id) ([pat] ++ argsPat) TypeConstrEmpty expr (ECExpr $ ExprVar id))
  unmarkTrace ast
  addExprAnnot $ return r
simplifyComplexExpression fn (ECLetOperator recK opName patArgs letExpr expr) = do
  --Ident $ getOperatorName opName
  addExprAnnot $ simplifyComplexExpression fn (ECLet recK (PatIdent $ Ident $ getOperatorName opName) patArgs TypeConstrEmpty letExpr expr)
simplifyComplexExpression fn ast@(ECLet recK pat [] typeAnnot letExpr expr) = do
  markTrace ast
  expr <- withTypeAnnot typeAnnot expr
  letSimpl <- simplifyComplexExpression fn letExpr
  exprSimpl <- simplifyComplexExpression fn expr
  r <- simplifyPattern fn (isRec recK) pat letSimpl exprSimpl
  unmarkTrace ast
  addExprAnnot $ return r
simplifyComplexExpression fn (ECChecked expr typeExpr) = do
  simpl <- simplifyComplexExpression fn expr
  s <- fn typeExpr
  addExprAnnot $ return $ Check simpl s
simplifyComplexExpression fn ast@(ECLet recK pat argsPats typeAnnot letExpr expr) = do
  markTrace ast
  expr <- withTypeAnnot typeAnnot expr
  letSimpl <- simplifyComplexExpression fn letExpr
  exprSimpl <- simplifyComplexExpression fn expr
  letSimplAcc <- foldrM (\pat expr -> do
    id <- freshIdent
    s <- simplifyPattern fn False pat (Var $ id) expr
    return $ Lam id s) letSimpl argsPats
  r <- simplifyPattern fn (isRec recK) pat letSimplAcc exprSimpl
  unmarkTrace ast
  addExprAnnot $ return r

simplifyTuple :: InferenceFn -> DTuple -> Infer Expr
simplifyTuple fn (DTuple firstElem elems) = do
  elemsSimpl <- foldM (\expr (DTupleElement el) -> do
    k <- simplifyExpression fn el
    return $ Op OpTupleCons k expr) (UniOp OpEmptyTuple Skip) ([firstElem] ++ elems)
  addExprAnnot $ return $ elemsSimpl

simplifyList :: InferenceFn -> DList -> Infer Expr
simplifyList fn (DList elems) = do
  elemsSimpl <- foldM (\expr (ListElement el) -> do
    k <- simplifyComplexExpression fn el
    return $ Op OpCons k expr) (UniOp OpEmptyList Skip) elems
  addExprAnnot $ return $ elemsSimpl

simplifyExpression :: InferenceFn -> Expression -> Infer Expr
simplifyExpression fn (ExprSemi expA expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op OpSemicolon simplA simplB
simplifyExpression _ (ExprOp opName) = do
  addExprAnnot $ return $ Var $ Ident $ getOperatorName opName
simplifyExpression fn (Expr1 expA (OperatorA name) expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression fn (Expr2 expA (OperatorB name) expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression fn (Expr3 expA (OperatorC name) expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression fn (Expr4 expA (OperatorD name) expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression fn (Expr4S expA OperatorDS expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op (OpCustom "*") simplA simplB
simplifyExpression fn (Expr5 expA (OperatorE name) expB) = do
  simplA <- simplifyExpression fn expA
  simplB <- simplifyExpression fn expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression fn (Expr6 (OperatorF name) exp) = do
  simpl <- simplifyExpression fn exp
  addExprAnnot $ return $ UniOp (OpCustomUni name) simpl
simplifyExpression _ (ExprVar name) = addExprAnnot $ return $ Var name
simplifyExpression _ (ExprConst (CInt val)) = addExprAnnot $ return $ Lit $ LInt val
simplifyExpression _ (ExprConst (CString val)) = addExprAnnot $ return $ Lit $ LString val
simplifyExpression _ (ExprConst (CBool CBTrue)) = addExprAnnot $ return $ Lit $ LBool True
simplifyExpression _ (ExprConst (CBool CBFalse)) = addExprAnnot $ return $ Lit $ LBool False
simplifyExpression fn (ExprList list) = addExprAnnot $ simplifyList fn list
simplifyExpression fn (ExprCompl expr) = addExprAnnot $ simplifyComplexExpression fn expr
simplifyExpression fn ast@(ExprCall exp firstArg restArgs) = do
  markTrace ast
  fnExpr <- simplifyExpression fn exp
  r <- foldlM (\simpl argExp -> do
    argSimpl <- simplifySimpleExpression fn argExp
    return $ App simpl argSimpl) (fnExpr) ([firstArg] ++ restArgs)
  unmarkTrace ast
  addExprAnnot $ return r

simplifySimpleExpression :: InferenceFn -> SimpleExpression -> Infer Expr
simplifySimpleExpression fn (ESOp opName) = simplifyExpression fn $ ExprOp opName
simplifySimpleExpression fn (ESConst const) = simplifyExpression fn $ ExprConst const
simplifySimpleExpression fn (ESIdent name) = simplifyExpression fn $ ExprVar name
simplifySimpleExpression fn (ESExpr expr) = simplifyComplexExpression fn expr
simplifySimpleExpression fn (ESList list) = simplifyList fn list
