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

execComplexExpression :: ComplexExpression -> Exec (RuntimeValue, Environment)
execComplexExpression (ECIf cond exp1 exp2) = do
  (condVal, condEnv) <- execComplexExpression cond >>= unpackBool
  if condVal then (local (\_ -> condEnv) $ execComplexExpression exp1) else (local (\_ -> condEnv) $ execComplexExpression exp2)
execComplexExpression (ECWhile cond exp) = do
  (condVal, condEnv) <- execComplexExpression cond >>= unpackBool
  if condVal then (local (\_ -> condEnv) $ execComplexExpression (ECWhile cond exp)) else (return (REmpty, condEnv))
execComplexExpression (ECFor name expVal1 dir expVal2 exp) = do
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
execComplexExpression (ECExpr expr) = execExpression expr
execComplexExpression (ECLet pattern [] letExpr expr) = do
  (letVal, letEnv) <- execComplexExpression letExpr
  (val, valEnv) <- local (\_ -> setPattern pattern letVal letEnv) $ (execComplexExpression expr)
  return $ (val, valEnv)
execComplexExpression (ECLet (PatIdent name) restPatterns letExpr expr) = do
  argsCount <- return $ length restPatterns
  fnBody <- return $ \args ->
    local (setPatterns restPatterns args) $ (execComplexExpression letExpr)
  (val, valEnv) <- local (createFunction name (RFunSig argsCount) fnBody) $ (execComplexExpression expr)
  return $ (val, valEnv)

execSimpleExpression :: SimpleExpression -> Exec (RuntimeValue, Environment)
execSimpleExpression (ESConst c) = execExpression $ ExprConst c
execSimpleExpression (ESIdent name) = do
  val <- ask >>= pullVariable name
  env <- ask
  return (val, env)
execSimpleExpression (ESExpr expr) = execExpression expr

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
execExpression (ExprCall name []) = do
  val <- ask >>= pullVariable name
  env <- ask
  return (val, env)
execExpression (ExprCall name argsExprs) = do
  env <- ask
  (args, argsEnv) <- foldM (\(res, env) exp -> do
    (r, newEnv) <- local (\_ -> env) $ execSimpleExpression exp
    return ((res ++ [r]), newEnv)) ([], env) argsExprs
  callFunction name args argsEnv

execExpression (ExprConst (CInt value)) = do
  env <- ask
  return ((RInt value), env)
execExpression (ExprConst (CString value)) = do
  env <- ask
  return ((RString value), env)
execExpression (ExprConst (CBool CBTrue)) = do
  env <- ask
  return ((RBool True), env)
execExpression (ExprConst (CBool CBFalse)) = do
  env <- ask
  return ((RBool False), env)
execExpression (Expr6 OpNot exp) = execExprOn exp $ vmapBool (\x -> RBool $ not x)
execExpression (Expr6 OpNeg exp) = execExprOn exp $ vmapInt (\x -> RInt $ -x)
execExpression (Expr5 exp1 OpMul exp2) = execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ x*y)
execExpression (Expr5 exp1 OpDiv exp2) = execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ div x y)
execExpression (Expr4 exp1 OpAdd exp2) = execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ x+y)
execExpression (Expr4 exp1 OpSub exp2) = execExprOn2 exp1 exp2 $ vmapInt2 (\x y -> RInt $ x-y)
execExpression (Expr3 exp1 OpEq exp2) = execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueEq val1 val2 >>= \r -> return $ RBool r
execExpression (Expr3 exp1 OpLt exp2) = execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueLt val1 val2 >>= \r -> return $ RBool r
execExpression (Expr3 exp1 OpGt exp2) = execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueGt val1 val2 >>= \r -> return $ RBool r
execExpression (Expr3 exp1 OpLtEq exp2) = execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueLtEq val1 val2 >>= \r -> return $ RBool r
execExpression (Expr3 exp1 OpGtEq exp2) = execExprOnUnpack2 exp1 exp2 $ \val1 val2 -> valueGtEq val1 val2 >>= \r -> return $ RBool r
execExpression (Expr2 exp1 OpAnd exp2) = execExprOn2 exp1 exp2 $ vmapBool2 (\x y -> RBool $ x && y)
execExpression (Expr1 exp1 OpOr exp2) = execExprOn2 exp1 exp2 $ vmapBool2 (\x y -> RBool $ x || y)