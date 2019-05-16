module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Environment
import Runtime
import Printer
import AbsSyntax

type Exec = StateT (Environment) (ReaderT (Environment) (ExceptT String IO))
type ProgramFn = Environment -> Exec RuntimeValue

data Program = Valid ProgramFn | Invalid (String)
data InterpreterState = EmptyState | ValueState

type Eval = StateT (InterpreterState) (ReaderT (Environment) (ExceptT String IO))

evalNotSupported :: (Show tree) => tree -> Eval ProgramFn
evalNotSupported tree = do
  throwError ("SyntaxError: Unsupported expression: " ++ (show tree))
  return (\env -> do
    return RUnit)

callNotSupported :: (Show tree) => tree -> Exec RuntimeValue
callNotSupported tree = do
  throwError ("DynamicCall: Call not supported: " ++ (show tree))
  return RUnit

callInfixOperator :: InfixOperator -> RuntimeValue -> RuntimeValue -> Exec RuntimeValue
callInfixOperator (OPPlus) (RInt a) (RInt b) = do
  return (RInt (a + b))
callInfixOperator (OPMinus) (RInt a) (RInt b) = do
  return (RInt (a - b))
callInfixOperator (OPMul) (RInt a) (RInt b) = do
  return (RInt (a * b))
callInfixOperator (OPDiv) (RInt a) (RInt b) = do
  return (RInt (quot a b))
callInfixOperator op _ _ = callNotSupported op

eval :: Implementation -> Eval ProgramFn
eval (IRootComplex a b) = do
  expA <- eval a
  expB <- eval b
  return (\env -> do
    valA <- expA emptyEnv
    valB <- expB emptyEnv
    return valB)
eval (IRoot a) = do
  exp <- evalImplPhrase a
  return (\env -> do
      val <- exp emptyEnv
      return val)
eval tree = evalNotSupported tree


evalImplPhrase :: ImplPhrase -> Eval ProgramFn
evalImplPhrase (IPhrase expr) = do
  exp <- evalExpression expr
  return (\env -> do
     val <- exp emptyEnv
     return val)
evalImplPhrase tree = evalNotSupported tree


evalExpression :: Expression -> Eval ProgramFn
evalExpression (EConst (CInt val)) = do
    return (\env -> do
        return (RInt val))
evalExpression (EConst (CString val)) = do
    return (\env -> do
        return (RString val))
evalExpression (EParens expr) = do
    exp <- evalExpression expr
    return (\env -> do
       val <- exp emptyEnv
       return val)
evalExpression (EComplex (ENInfix exprA op exprB)) = do
    expA <- evalExpression exprA
    expB <- evalExpression exprB
    return (\env -> do
       valA <- expA emptyEnv
       valB <- expB emptyEnv
       res <- callInfixOperator op valA valB
       return res)
evalExpression tree = evalNotSupported tree

---


runInterpretAST tree env = runExceptT (runReaderT (runStateT (eval tree) (EmptyState)) (env))

interpretAST tree env = do
  e <- runInterpretAST tree env
  program <- return (case e of
    Left s -> Invalid s
    Right (e, _) -> Valid e)
  return program

execProgram :: ProgramFn -> Environment -> IO ProgramResult
execProgram prog env = do
  e <- runExceptT (runReaderT (runStateT (prog env) (env)) (env))
  result <- return (case e of
    Left err -> FailedExecution err
    Right (res, state) -> Executed res state)
  return result

runAST :: Implementation -> Environment -> IO ProgramResult
runAST tree env = do
  program <- interpretAST tree env
  result <- (case program of
    Valid p -> execProgram p env
    Invalid s -> return (FailedParse s))
  return result