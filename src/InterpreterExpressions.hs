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

import AbsSyntax

execList :: DList -> Exec (RuntimeValue, Environment)
execList ast@(DList listElements) = do
  proceedD ast
  env <- ask
  (listItems, listEnv) <- foldM (\(res, env) (ListElement exp) -> do
      (r, newEnv) <- local (\_ -> env) $ execComplexExpression exp
      return ((res ++ [r]), newEnv)) ([], env) listElements
  return $ ((RList listItems), listEnv)

execTuple :: DTuple -> Exec (RuntimeValue, Environment)
execTuple ast@(DTuple firstElement tupleElements) = do
  proceedD ast
  env <- ask
  (tupleItems, tupleEnv) <- foldM (\(res, env) (DTupleElement exp) -> do
      (r, newEnv) <- local (\_ -> env) $ execExpression exp
      return ((res ++ [r]), newEnv)) ([], env) (firstElement:tupleElements)
  return $ ((RTuple tupleItems), tupleEnv)

execComplexExpression :: ComplexExpression -> Exec (RuntimeValue, Environment)
execComplexExpression (ECTuple tuple) = execTuple tuple
execComplexExpression ast@(ECIf cond exp1 exp2) = do
  proceed ast
  (condVal, condEnv) <- execComplexExpression cond >>= unpackBool
  if condVal then (local (\_ -> condEnv) $ execComplexExpression exp1) else (local (\_ -> condEnv) $ execComplexExpression exp2)
execComplexExpression ast@(ECWhile cond exp) = do
  proceed ast
  (condVal, condEnv) <- execComplexExpression cond >>= unpackBool
  if condVal then (local (\_ -> condEnv) $ execComplexExpression (ECWhile cond exp)) else (return (REmpty, condEnv))
execComplexExpression ast@(ECFor name expVal1 dir expVal2 exp) = do
  proceed ast
  (val1, env1) <- execComplexExpression expVal1 >>= unpackInt
  (val2, env2) <- local (\_ -> env1) $ execComplexExpression expVal2 >>= unpackInt
  case dir of
    ForDirTo -> if val1 < val2 then
        local (\_ -> env2) $ execComplexExpression (ECFor name (ECExpr $ ExprConst $ CInt $ val1 + 1) dir expVal2 exp)
      else
        return $ (REmpty, env2)
    ForDirDownTo -> if val1 > val2 then
        local (\_ -> env2) $ execComplexExpression (ECFor name (ECExpr $ ExprConst $ CInt $ val1 - 1) dir expVal2 exp)
      else
        return $ (REmpty, env2)
execComplexExpression ast@(ECExpr expr) = do
  proceed ast
  execExpression expr
execComplexExpression ast@(ECLet pattern [] letExpr expr) = do
  proceed ast
  (letVal, letEnv) <- execComplexExpression letExpr
  (val, valEnv) <- local (\_ -> setPattern pattern letVal letEnv) $ (execComplexExpression expr)
  return $ (val, valEnv)
execComplexExpression ast@(ECLet (PatIdent name) restPatterns letExpr expr) = do
  proceed ast
  argsCount <- return $ length restPatterns
  fnBody <- return $ \args ->
    local (setPatterns restPatterns args) $ (execComplexExpression letExpr)
  (val, valEnv) <- local (createFunction name (RFunSig argsCount) fnBody) $ (execComplexExpression expr)
  return $ (val, valEnv)
execComplexExpression ast@(ECFun pattern restPatterns bodyExpr) = do
  proceed ast
  argsCount <- return $ 1 + (length restPatterns)
  fnBody <- return $ \args ->
    local (setPatterns ([pattern] ++ restPatterns) args) $ (execComplexExpression bodyExpr)
  env <- ask
  return $ newFunction (RFunSig argsCount) fnBody env

execSimpleExpression :: SimpleExpression -> Exec (RuntimeValue, Environment)
execSimpleExpression (ESConst c) = execExpression $ ExprConst c
execSimpleExpression (ESIdent name) = do
  val <- ask >>= pullVariable name
  env <- ask
  return (val, env)
execSimpleExpression (ESExpr expr) = execExpression expr
execSimpleExpression (ESList list) = execList list

execExprOn :: Expression -> (Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)) -> Exec (RuntimeValue, Environment)
execExprOn exp fn = do
  (val, env) <- execExpression exp
  (valFn, valEnv) <- fn (return (val, env))
  return (valFn, valEnv)

execExprOn2 :: Expression -> Expression -> (Exec (RuntimeValue, Environment)-> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)) -> Exec (RuntimeValue, Environment)
execExprOn2 exp1 exp2 fn = do
  (val1, env1) <- execExpression exp1
  (val2, env2) <- local (\_ -> env1) $ execExpression exp2
  (valFn, valEnv) <- fn (return (val1,env1)) (return (val2,env2))
  return (valFn, valEnv)

execExprOnUnpack :: Expression -> (RuntimeValue -> Exec RuntimeValue) -> Exec (RuntimeValue, Environment)
execExprOnUnpack exp fn = do
  (val, env) <- execExpression exp
  r <- fn val
  return (r, env)

execExprOnUnpack2 :: Expression -> Expression -> (RuntimeValue -> RuntimeValue -> Exec RuntimeValue) -> Exec (RuntimeValue, Environment)
execExprOnUnpack2 exp1 exp2 fn = do
  (val1, env1) <- execExpression exp1
  (val2, env2) <- local (\_ -> env1) $ execExpression exp2
  r <- fn val1 val2
  return (r, env2)

execExpression :: Expression -> Exec (RuntimeValue, Environment)
execExpression (ExprCompl expr) = execComplexExpression expr
execExpression (ExprList list) = execList list
execExpression ast@(ExprCall name []) = do
  proceedD ast
  val <- ask >>= pullVariable name
  env <- ask
  return (val, env)
execExpression ast@(ExprCall name argsExprs) = do
  proceedD ast
  env <- ask
  (args, argsEnv) <- foldM (\(res, env) exp -> do
    (r, newEnv) <- local (\_ -> env) $ execSimpleExpression exp
    return ((res ++ [r]), newEnv)) ([], env) argsExprs
  callFunction name args argsEnv
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
execExpression ast@(Expr6 OpNot exp) = proceedT ast $ execExprOn exp $ vmapBool (\x -> RBool $ not x)
execExpression ast@(Expr6 OpNeg exp) = proceedT ast $ execExprOn exp $ vmapInt (\x -> RInt $ -x)
execExpression ast@(Expr5 exp1 OpMul exp2) = proceedT ast $ execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ x*y)
execExpression ast@(Expr5 exp1 OpDiv exp2) = proceedT ast $ execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ div x y)
execExpression ast@(Expr4 exp1 OpAdd exp2) = proceedT ast $ execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ x+y)
execExpression ast@(Expr4 exp1 OpSub exp2) = proceedT ast $ execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ x-y)
execExpression ast@(Expr3 exp1 OpEq exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueEq val1 val2 >>= \r -> return $ RBool r
execExpression ast@(Expr3 exp1 OpLt exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueLt val1 val2 >>= \r -> return $ RBool r
execExpression ast@(Expr3 exp1 OpGt exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueGt val1 val2 >>= \r -> return $ RBool r
execExpression ast@(Expr3 exp1 OpLtEq exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueLtEq val1 val2 >>= \r -> return $ RBool r
execExpression ast@(Expr3 exp1 OpGtEq exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueGtEq val1 val2 >>= \r -> return $ RBool r
execExpression ast@(Expr2 exp1 OpCons exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueCons val1 val2
execExpression ast@(Expr2 exp1 OpJoin exp2) = proceedT ast $ execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueJoin val1 val2
execExpression ast@(Expr2 exp1 OpAnd exp2) = proceedT ast $ execExprOn2 exp1 exp2 $ vmapBool2 (\x y -> RBool $ x && y)
execExpression ast@(Expr1 exp1 OpOr exp2) = proceedT ast $ execExprOn2 exp1 exp2 $ vmapBool2 (\x y -> RBool $ x || y)