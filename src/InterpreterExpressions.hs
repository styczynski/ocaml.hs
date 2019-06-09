module InterpreterExpressions where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import InterpreterDefinitions
import Runtime
import Arithmetics
import Environment
import InterpreterPatterns
import qualified Data.Map as Map

import AbsSyntax

execRecord :: DataRecord -> Exec (RuntimeValue, Environment)
execRecord ast@(DataRecord recordElements) = do
  proceedD ast
  env <- ask
  (recordFields, recordEnv, fieldNames) <- foldM (\(res, env, names) (RecordElement fieldName exp) -> do
        (r, newEnv) <- shadow env $ local (\_ -> env) $ execComplexExpression exp
        return ((Map.insert fieldName r res), newEnv, [fieldName] ++ names)) (Map.empty, env, []) recordElements
  recordTypeName <- return $ findRecordDefByFieldNames fieldNames recordEnv
  case recordTypeName of
    DInvalid -> raise $ "Invalid field name was specified for record type: " ++ (treeToStr ast)
    (DRecord name _) -> return $ ((RRecord name recordFields), recordEnv)

execList :: DList -> Exec (RuntimeValue, Environment)
execList ast@(DList listElements) = do
  proceedD ast
  env <- ask
  (listItems, listEnv) <- foldM (\(res, env) (ListElement exp) -> do
      (r, newEnv) <- shadow env $ local (\_ -> env) $ execComplexExpression exp
      return ((res ++ [r]), newEnv)) ([], env) listElements
  return $ ((RList listItems), listEnv)

execTuple :: DTuple -> Exec (RuntimeValue, Environment)
execTuple ast@(DTuple firstElement tupleElements) = do
  proceedD ast
  env <- ask
  (tupleItems, tupleEnv) <- foldM (\(res, env) (DTupleElement exp) -> do
      (r, newEnv) <- shadow env $ local (\_ -> env) $ execExpression exp
      return ((res ++ [r]), newEnv)) ([], env) (firstElement:tupleElements)
  return $ ((RTuple tupleItems), tupleEnv)

execComplexExpression :: ComplexExpression -> Exec (RuntimeValue, Environment)
execComplexExpression (ECTuple tuple) = execTuple tuple
execComplexExpression ast@(ECIf cond exp1 exp2) = do
  proceed ast
  env <- ask
  (condVal, condEnv) <- shadow env $ execComplexExpression cond >>= unpackBool
  if condVal then (shadow env $ local (\_ -> condEnv) $ execComplexExpression exp1) else (shadow env $ local (\_ -> condEnv) $ execComplexExpression exp2)
execComplexExpression ast@(ECWhile cond exp) = do
  proceed ast
  env <- ask
  (condVal, condEnv) <- shadow env $ execComplexExpression cond >>= unpackBool
  if condVal then (shadow env $ local (\_ -> condEnv) $ execComplexExpression (ECWhile cond exp)) else (return (REmpty, condEnv))
execComplexExpression ast@(ECFor name expVal1 dir expVal2 exp) = do
  proceed ast
  env <- ask
  (val1, env1) <- shadow env $ execComplexExpression expVal1 >>= unpackInt
  (val2, env2) <- shadow env $ local (\_ -> env1) $ execComplexExpression expVal2 >>= unpackInt
  case dir of
    ForDirTo -> if val1 < val2 then
        shadow env $ local (\_ -> env2) $ execComplexExpression (ECFor name (ECExpr $ ExprConst $ CInt $ val1 + 1) dir expVal2 exp)
      else
        shadow env $ return $ (REmpty, env2)
    ForDirDownTo -> if val1 > val2 then
        shadow env $ local (\_ -> env2) $ execComplexExpression (ECFor name (ECExpr $ ExprConst $ CInt $ val1 - 1) dir expVal2 exp)
      else
        shadow env $ return $ (REmpty, env2)
execComplexExpression ast@(ECMatch expr _ clauses) = do
  env <- ask
  (expVal, expEnv) <- shadow env $ execComplexExpression expr
  (matchEnv, matchRes) <- setMatchPatterns (map (\(MatchClause pat patExp) -> (pat, patExp)) clauses) expVal expEnv
  case matchRes of
    Nothing -> raise $ "Not-exhaustive match exception"
    Just exp -> do
      shadow env $ local (\_ -> matchEnv) $ execComplexExpression exp
execComplexExpression ast@(ECFunction bPip matchClauses) = do
  proceed ast
  env <- ask
  shadow env $ execComplexExpression $ ECFun (PatIdent (Ident "x")) [] $ ECMatch (ECExpr $ ExprCall (Ident "x") []) bPip matchClauses
execComplexExpression ast@(ECExpr expr) = do
  proceed ast
  env <- ask
  shadow env $ execExpression expr
execComplexExpression ast@(ECLetOperator _ opAny pattern restPatterns letExpr expr) = do
  proceed ast
  env <- ask
  argsCount <- return $ 1 + length restPatterns
  fnBody <- return $ \args -> do
    inEnv <- ask
    patEnv <- setPatterns ([pattern] ++ restPatterns) args inEnv
    shadow inEnv $ local (\_ -> patEnv) $ (execComplexExpression letExpr)
  (_, defEnv) <- createOperator opAny (RFunSig argsCount) fnBody
  (val, valEnv) <- shadow env $ local (\_ -> defEnv) $ (execComplexExpression expr)
  return $ (val, valEnv)
execComplexExpression ast@(ECLet _ pattern [] letExpr expr) = do
  proceed ast
  env <- ask
  (letVal, letEnv) <- shadow env $ execComplexExpression letExpr
  patEnv <- setPattern pattern letVal letEnv
  (val, valEnv) <- shadow env $ local (\_ -> patEnv) $ (execComplexExpression expr)
  return $ (val, valEnv)
execComplexExpression ast@(ECLet _ (PatIdent name) restPatterns letExpr expr) = do
  proceed ast
  env <- ask
  argsCount <- return $ length restPatterns
  fnBody <- return $ \args -> do
    inEnv <- ask
    patEnv <- setPatterns restPatterns args inEnv
    shadow inEnv $ local (\_ -> patEnv) $ (execComplexExpression letExpr)
  (val, valEnv) <- shadow env $ local (createFunction name (RFunSig argsCount) fnBody) $ (execComplexExpression expr)
  return $ (val, valEnv)
execComplexExpression ast@(ECFun pattern restPatterns bodyExpr) = do
  proceed ast
  env <- ask
  argsCount <- return $ 1 + (length restPatterns)
  fnBody <- return $ \args -> do
    inEnv <- ask
    patEnv <- setPatterns ([pattern] ++ restPatterns) args inEnv
    shadow inEnv $ local (\_ -> patEnv) $ (execComplexExpression bodyExpr)
  shadow env $ return $ newFunction (RFunSig argsCount) fnBody env

execSimpleExpression :: SimpleExpression -> Exec (RuntimeValue, Environment)
execSimpleExpression (ESRecord record) = execRecord record
execSimpleExpression (ESConst c) = execExpression $ ExprConst c
execSimpleExpression (ESIdent name) = do
  val <- ask >>= pullVariable name
  env <- ask
  return (val, env)
execSimpleExpression (ESExpr expr) = execComplexExpression expr
execSimpleExpression (ESList list) = execList list

execExpression :: Expression -> Exec (RuntimeValue, Environment)
execExpression (ExprRecord record) = execRecord record
execExpression (ExprCompl expr) = execComplexExpression expr
execExpression (ExprList list) = execList list
execExpression (ExprSel expr name) = do
  env <- ask
  (val, env) <- shadow env $ execExpression expr
  retVal <- valueSel val name
  return (retVal, env)
execExpression ast@(ExprCall name []) = do
  proceedD ast
  val <- ask >>= pullVariable name
  env <- ask
  return (val, env)
execExpression ast@(ExprCall name argsExprs) = do
  proceedD ast
  env <- ask
  (args, argsEnv) <- shadow env $ foldM (\(res, env) exp -> do
    (r, newEnv) <- shadow env $ local (\_ -> env) $ execSimpleExpression exp
    return ((res ++ [r]), newEnv)) ([], env) argsExprs
  shadow env $ callFunction name args argsEnv
execExpression ast@(ExprConst (CInt value)) = do
  proceedD ast
  env <- ask
  return ((RInt value), env)
execExpression ast@(ExprConst (CString value)) = do
  proceedD ast
  env <- ask
  return ((RString value), env)
execExpression ast@(ExprConst (CBool CBTrue)) = do
  proceedD ast
  env <- ask
  return ((RBool True), env)
execExpression ast@(ExprConst (CBool CBFalse)) = do
  proceedD ast
  env <- ask
  return ((RBool False), env)
execExpression ast@(Expr6 op exp) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp
  shadow env $ local (\_ -> env1) $ callOperatorF op val1
execExpression ast@(Expr5 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  shadow env $ local (\_ -> env2) $ callOperatorE op val1 val2
execExpression ast@(Expr4 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  shadow env $ local (\_ -> env2) $ callOperatorD op val1 val2
execExpression ast@(Expr4S exp1 OperatorDS exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  shadow env $ local (\_ -> env2) $ callOperatorDS val1 val2
execExpression ast@(Expr3 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  shadow env $ local (\_ -> env2) $ callOperatorC op val1 val2
execExpression ast@(Expr2 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  shadow env $ local (\_ -> env2) $ callOperatorB op val1 val2
execExpression ast@(Expr1 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  shadow env $ local (\_ -> env2) $ callOperatorA op val1 val2