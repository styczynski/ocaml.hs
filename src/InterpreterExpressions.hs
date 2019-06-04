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

execComplexExpression :: ComplexExpression -> Exec RuntimeValue
execComplexExpression (ECExpr expr) = execExpression expr
execComplexExpression (ECLet pattern restPatterns letExpr expr) = do
  letVal <- execComplexExpression letExpr
  val <- local (setPattern pattern letVal) $ (execComplexExpression expr)
  return $ val

execExprOn :: Expression -> (Exec RuntimeValue -> Exec RuntimeValue) -> Exec RuntimeValue
execExprOn exp fn = fn $ execExpression exp

execExprOn2 :: Expression -> Expression -> (Exec RuntimeValue -> Exec RuntimeValue -> Exec RuntimeValue) -> Exec RuntimeValue
execExprOn2 exp1 exp2 fn = fn (execExpression exp1) (execExpression exp2)

execExprOnUnpack :: Expression -> (RuntimeValue -> Exec RuntimeValue) -> Exec RuntimeValue
execExprOnUnpack exp fn = do
  val <- execExpression exp
  r <- fn val
  return $ r

execExprOnUnpack2 :: Expression -> Expression -> (RuntimeValue -> RuntimeValue -> Exec RuntimeValue) -> Exec RuntimeValue
execExprOnUnpack2 exp1 exp2 fn = do
  val1 <- execExpression exp1
  val2 <- execExpression exp2
  r <- fn val1 val2
  return $ r

execExpression :: Expression -> Exec RuntimeValue
execExpression (ExprCall name args) =
  ask >>= pullVariable name

execExpression (ExprConst (CInt value)) = return $ RInt value
execExpression (ExprConst (CString value)) = return $ RString value
execExpression (ExprConst (CBool CBTrue)) = return $ RBool True
execExpression (ExprConst (CBool CBFalse)) = return $ RBool False
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