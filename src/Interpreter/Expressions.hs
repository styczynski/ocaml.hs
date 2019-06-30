module Interpreter.Expressions where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime.Runtime
import Runtime.Environment

import Interpreter.Definitions
import Interpreter.Patterns

import Arithmetics
import qualified Data.Map as Map

import Syntax.Base

isRec :: LetRecKeyword -> Bool
isRec LetRecYes = True
isRec LetRecNo = False

execRecord :: DataRecord -> Exec (RuntimeValue, Environment)
execRecord ast@(DataRecord recordElements) = do
  proceed ast
  env <- ask
  (recordFields, recordEnv, fieldNames) <- foldM (\(res, env, names) (RecordElement fieldName exp) -> do
        (r, newEnv) <- shadow env $ local (\_ -> env) $ execComplexExpression exp
        return ((Map.insert fieldName r res), newEnv, [fieldName] ++ names)) (Map.empty, env, []) recordElements
  recordTypeName <- return $ findRecordDefByFieldNames fieldNames recordEnv
  unproceed
  case recordTypeName of
    DInvalid -> raise $ "Invalid field name was specified for record type: " ++ (treeToStr ast)
    (DRecord name _) -> return $ ((RRecord name recordFields), recordEnv)

execList :: DList -> Exec (RuntimeValue, Environment)
execList ast@(DList listElements) = do
  proceed ast
  env <- ask
  (listItems, listEnv) <- foldM (\(res, env) (ListElement exp) -> do
      (r, newEnv) <- shadow env $ local (\_ -> env) $ execComplexExpression exp
      return ((res ++ [r]), newEnv)) ([], env) listElements
  unproceed
  return $ ((RList listItems), listEnv)

execTuple :: DTuple -> Exec (RuntimeValue, Environment)
execTuple ast@(DTuple firstElement tupleElements) = do
  proceed ast
  env <- ask
  (tupleItems, tupleEnv) <- foldM (\(res, env) (DTupleElement exp) -> do
      (r, newEnv) <- shadow env $ local (\_ -> env) $ execExpression exp
      return ((res ++ [r]), newEnv)) ([], env) (firstElement:tupleElements)
  unproceed
  return $ ((RTuple tupleItems), tupleEnv)

execComplexExpression :: ComplexExpression -> Exec (RuntimeValue, Environment)
execComplexExpression ECExportEnv = do
  env <- ask
  return (RExport env, env)
execComplexExpression (ECTuple tuple) = execTuple tuple
execComplexExpression ast@(ECIf cond exp1 exp2) = do
  proceed ast
  env <- ask
  (condVal, condEnv) <- shadow env $ execComplexExpression cond >>= unpackBool
  r <- if condVal then (shadow env $ local (\_ -> condEnv) $ execComplexExpression exp1) else (shadow env $ local (\_ -> condEnv) $ execComplexExpression exp2)
  unproceed
  return r
execComplexExpression ast@(ECWhile cond exp) = do
  proceed ast
  env <- ask
  (condVal, condEnv) <- shadow env $ execComplexExpression cond >>= unpackBool
  r <- (if condVal then do
          (_, stEnv) <- shadow env $ local (\_ -> condEnv) $ execComplexExpression exp
          (shadow env $ local (\_ -> stEnv) $ execComplexExpression (ECWhile cond exp))
        else (return (REmpty, condEnv)))
  unproceed
  return r
execComplexExpression ast@(ECFor name expVal1 dir expVal2 exp) = do
  proceed ast
  env <- ask
  (val1, env1) <- shadow env $ execComplexExpression expVal1 >>= unpackInt
  (val2, env2) <- shadow env $ local (\_ -> env1) $ execComplexExpression expVal2 >>= unpackInt
  env2i <- return $ setVariable name (RInt val1) env2
  f <- case dir of
    ForDirTo -> if val1 <= val2 then do
        (_, env3) <- shadow env $ local (\_ -> env2i) $ execComplexExpression exp
        shadow env $ local (\_ -> env3) $ execComplexExpression (ECFor name (ECExpr $ ExprConst $ CInt $ val1 + 1) dir expVal2 exp)
      else
        shadow env $ return $ (REmpty, env2)
    ForDirDownTo -> if val1 >= val2 then do
        (_, env3) <- shadow env $ local (\_ -> env2i) $ execComplexExpression exp
        shadow env $ local (\_ -> env3) $ execComplexExpression (ECFor name (ECExpr $ ExprConst $ CInt $ val1 - 1) dir expVal2 exp)
      else
        shadow env $ return $ (REmpty, env2)
  unproceed
  return f
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
  (id, env) <- return $ allocIdent env
  unproceed
  shadow env $ execComplexExpression $ ECFun (PatIdent id) [] $ ECMatch (ECExpr $ ExprVar id) bPip matchClauses
execComplexExpression ast@(ECExpr expr) = do
  proceed ast
  env <- ask
  unproceed
  shadow env $ execExpression expr
execComplexExpression ast@(ECLet LetRecNo pattern [] typeAnnot letExpr expr) = do
  proceed ast
  validatePatterns ([pattern])
  env <- ask
  (letVal, letEnv) <- shadow env $ execComplexExpression letExpr
  patEnv <- local (\_ -> letEnv) $ setPattern pattern letVal letEnv
  (val, valEnv) <- shadow env $ local (\_ -> patEnv) $ (execComplexExpression expr)
  unproceed
  return $ (val, valEnv)
execComplexExpression ast@(ECLet r (PatIdent name) restPatterns typeAnnot letExpr expr) = do
  proceed ast
  validatePatterns ([(PatIdent name)] ++ restPatterns)
  env <- ask
  argsCount <- return $ length restPatterns
  newRef <- return $ allocRef env
  fnBody <- return $ \args -> do
    inEnv <- shadowE env ask
    inEnv <- return $ if isRec r then importEnvRef newRef name inEnv env else inEnv
    patEnv <- local (\_ -> inEnv) $ setPatterns restPatterns args inEnv
    (eVal, eEnv) <- shadow inEnv $ local (\_ -> patEnv) $ (execComplexExpression letExpr)
    return (eVal, eEnv)
  (val, valEnv) <- shadow env $ local (createFunction name (Just newRef) (RFunSig argsCount) fnBody) $ (execComplexExpression expr)
  unproceed
  return $ (val, valEnv)
execComplexExpression ast@(ECLet r pattern [] typeAnnot letExpr expr) = do
  proceed ast
  validatePatterns ([pattern])
  env <- ask
  (letVal, letEnv) <- shadow env $ execComplexExpression letExpr
  patEnv <- local (\_ -> letEnv) $ setPattern pattern letVal letEnv
  (val, valEnv) <- shadow env $ local (\_ -> patEnv) $ (execComplexExpression expr)
  unproceed
  return $ (val, valEnv)
execComplexExpression ast@(ECLetOperator r opAny restPatterns letExpr expr) = do
  proceed ast
  validatePatterns (restPatterns)
  env <- ask
  (id, env) <- return $ allocIdent env
  (valFnRef, env2) <- shadow env $ execComplexExpression (ECLet r (PatIdent id) restPatterns TypeConstrEmpty letExpr (ECExpr $ ExprVar id))
  (RfFun fnSig fnBody) <- return $ getRefStorage valFnRef env2
  (_, defEnv) <- local (\_ -> env2) $ createOperator opAny fnSig fnBody
  (val, valEnv) <- shadow env $ local (\_ -> defEnv) $ (execComplexExpression expr)
  unproceed
  return $ (val, valEnv)
execComplexExpression ast@(ECLet r (PatIdent name) restPatterns typeAnnot letExpr expr) = do
  proceed ast
  validatePatterns ([(PatIdent name)] ++ restPatterns)
  env <- ask
  newRef <- return $ allocRef env
  argsCount <- return $ length restPatterns
  fnBody <- return $ \args -> do
    inEnv <- shadowE env ask
    inEnv <- return $ if isRec r then importEnvRef newRef name inEnv env else inEnv
    patEnv <- local (\_ -> inEnv) $ setPatterns restPatterns args inEnv
    shadow inEnv $ local (\_ -> patEnv) $ (execComplexExpression letExpr)
  (val, valEnv) <- shadow env $ local (createFunction name (Just newRef) (RFunSig argsCount) fnBody) $ (execComplexExpression expr)
  unproceed
  return $ (val, valEnv)
execComplexExpression ast@(ECFun pattern restPatterns bodyExpr) = do
  proceed ast
  validatePatterns ([pattern] ++ restPatterns)
  env <- ask
  argsCount <- return $ 1 + (length restPatterns)
  fnBody <- return $ \args -> do
    inEnv <- shadowE env ask
    patEnv <- local (\_ -> inEnv) $ setPatterns ([pattern] ++ restPatterns) args inEnv
    shadow inEnv $ local (\_ -> patEnv) $ (execComplexExpression bodyExpr)
  unproceed
  shadow env $ return $ newFunction (RFunSig argsCount) fnBody env

execSimpleExpression :: SimpleExpression -> Exec (RuntimeValue, Environment)
execSimpleExpression (ESOp opName) = execExpression $ ExprOp opName
execSimpleExpression (ESRecord record) = execRecord record
execSimpleExpression (ESConst c) = execExpression $ ExprConst c
execSimpleExpression (ESIdent name) = do
  (val, env) <- ask >>= pullVariable name
  return (val, env)
execSimpleExpression (ESExpr expr) = execComplexExpression expr
execSimpleExpression (ESList list) = execList list

getOperatorName :: OperatorAny -> String
getOperatorName (OperatorAnyA (OperatorA name)) = name
getOperatorName (OperatorAnyB (OperatorB name)) = name
getOperatorName (OperatorAnyC (OperatorC name)) = name
getOperatorName (OperatorAnyD (OperatorD name)) = name
getOperatorName (OperatorAnyDS (OperatorDS)) = "*"
getOperatorName (OperatorAnyE (OperatorE name)) = name
getOperatorName (OperatorAnyF (OperatorF name)) = name


execExpression :: Expression -> Exec (RuntimeValue, Environment)
execExpression (ExprSemi action1 action2) = do
  env <- ask
  (_, env2) <- shadow env $ execExpression action1
  (val, env3) <- shadow env $ local (\_ -> env2) $ execExpression action2
  return (val, env3)
execExpression (ExprOp opName) = do
  env <- ask
  (val, env2) <- getVariable (Ident $ getOperatorName opName) env
  return (val, env2)
execExpression (ExprRecord record) = execRecord record
execExpression (ExprCompl expr) = execComplexExpression expr
execExpression (ExprList list) = execList list
execExpression (ExprSel expr name) = do
  env <- ask
  (val, env) <- shadow env $ execExpression expr
  retVal <- valueSel val name
  return (retVal, env)
execExpression ast@(ExprVar name) = do
  proceed ast
  (val, env) <- ask >>= pullVariable name
  unproceed
  return (val, env)
execExpression ast@(ExprCall expr1 argFirst argsRest) = do
  proceed ast
  env <- ask
  argsExprs <- return $ [argFirst] ++ argsRest
  (fn, fnEnv) <- shadow env $ execExpression expr1
  (args, argsEnv) <- shadow env $ local (\_ -> fnEnv) $ foldM (\(res, env) exp -> do
    (r, newEnv) <- shadow env $ local (\_ -> env) $ execSimpleExpression exp
    return ((res ++ [r]), newEnv)) ([], fnEnv) argsExprs
  unproceed
  shadow env $ local (\_ -> argsEnv) $ callFunctionR fn args argsEnv
execExpression ast@(ExprConst (CInt value)) = do
  proceed ast
  env <- ask
  unproceed
  return ((RInt value), env)
execExpression ast@(ExprConst (CString value)) = do
  proceed ast
  env <- ask
  unproceed
  return ((RString value), env)
execExpression ast@(ExprConst (CBool CBTrue)) = do
  proceed ast
  env <- ask
  unproceed
  return ((RBool True), env)
execExpression ast@(ExprConst (CBool CBFalse)) = do
  proceed ast
  env <- ask
  unproceed
  return ((RBool False), env)
execExpression ast@(Expr6 op exp) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp
  unproceed
  shadow env $ local (\_ -> env1) $ callOperatorF op val1
execExpression ast@(Expr5 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  unproceed
  shadow env $ local (\_ -> env2) $ callOperatorE op val1 val2
execExpression ast@(Expr4 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  unproceed
  shadow env $ local (\_ -> env2) $ callOperatorD op val1 val2
execExpression ast@(Expr4S exp1 OperatorDS exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  unproceed
  shadow env $ local (\_ -> env2) $ callOperatorDS val1 val2
execExpression ast@(Expr3 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  unproceed
  shadow env $ local (\_ -> env2) $ callOperatorC op val1 val2
execExpression ast@(Expr2 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  unproceed
  shadow env $ local (\_ -> env2) $ callOperatorB op val1 val2
execExpression ast@(Expr1 exp1 op exp2) = do
  proceed ast
  env <- ask
  (val1,env1) <- shadow env $ execExpression exp1
  (val2,env2) <- shadow env $ local (\_ -> env1) $ execExpression exp2
  unproceed
  shadow env $ local (\_ -> env2) $ callOperatorA op val1 val2

execPhrase :: ImplPhrase -> Exec (RuntimeValue, Environment)
execPhrase (IGlobalLet recK pattern restPatterns typeAnnot letExpr) = do
  (r, _) <- execComplexExpression (ECLet recK pattern restPatterns typeAnnot letExpr $ ECExportEnv)
  return $ let (RExport env) = r in (REmpty, env)
execPhrase (IGlobalLetOperator recK opName restPatterns letExpr) = do
  (r, _) <- execComplexExpression (ECLetOperator recK opName restPatterns letExpr $ ECExportEnv)
  return $ let (RExport env) = r in (REmpty, env)
execPhrase (IDefType typeDef) = execTypeDef typeDef