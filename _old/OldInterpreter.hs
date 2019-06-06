{-# LANGUAGE QuasiQuotes #-}
module Interpreter where

import Data.List
import Text.RawString.QQ
import Text.Format.Para

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Macros
import Environment
import Runtime
import Printer
import AbsSyntax

import qualified Data.Map as Map


data Program = Valid ProgramFn | Invalid (String)
data InterpreterState = EmptyState | ValueState

type Eval = StateT (InterpreterState) (ReaderT (Environment) (ExceptT String IO))

getPatternMapping :: Pattern -> RuntimeValue -> Map.Map Ident RuntimeValue
getPatternMapping (PatIdent name) val = Map.insert name val Map.empty

setPattern :: Environment -> Pattern -> RuntimeValue -> Environment
setPattern env pattern val = let pm = getPatternMapping pattern val in
  Map.foldrWithKey (\name val env -> setVariable env name val) env pm

setPatterns :: Environment -> [Pattern] -> [RuntimeValue] -> Environment
setPatterns env patterns vals = foldl (\env (p,v) -> setPattern env p v) env (zip patterns vals)

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
  return $ RInt $ a + b

callInfixOperator (OPMinus) (RInt a) (RInt b) = do
  return $ RInt $ a - b

callInfixOperator (OPMul) (RInt a) (RInt b) = do
  return $ RInt $ a * b

callInfixOperator (OPDiv) (RInt a) (RInt b) = do
  return $ RInt $ quot a b

callInfixOperator (OPLt) (RInt a) (RInt b) = do
  return $ RBool $ a < b

callInfixOperator (OPLt) (RInt a) (RInt b) = do
  return $ RBool $ a < b

callInfixOperator (OPEqLt) (RInt a) (RInt b) = do
  return $ RBool $ a <= b

callInfixOperator (OPGt) (RInt a) (RInt b) = do
  return $ RBool $ a > b

callInfixOperator (OPEqGt) (RInt a) (RInt b) = do
  return $ RBool $ a >= b

callInfixOperator (OPAnd) (RBool a) (RBool b) = do
  return $ RBool $ a && b

callInfixOperator (OPOr) (RBool a) (RBool b) = do
  return $ RBool $ a || b

callInfixOperator (OPEq) (RInt a) (RInt b) = do
  return $ RBool $ a == b

callInfixOperator (OPEqS) (RInt a) (RInt b) = do
  return $ RBool $ a == b

callInfixOperator op _ _ = callNotSupported op

eval :: Implementation -> Eval ProgramFn
eval INothing = do
  return $ \env -> do
    return RUnit
eval (IRootComplex a b) = do
  expA <- eval a
  expB <- eval b
  return $ \env -> do
    valA <- expA emptyEnv
    valB <- expB emptyEnv
    return valB
eval (IRoot a) = do
  exp <- evalImplPhrase a
  return $ \env -> do
      val <- exp emptyEnv
      return val
eval tree = evalNotSupported tree


evalImplPhrase :: ImplPhrase -> Eval ProgramFn
evalImplPhrase (IPhrase expr) = do
  exp <- evalExpression expr
  return $ \env -> do
     val <- exp emptyEnv
     return val
evalImplPhrase (IDef expr) = do
  exp <- evalDefinition expr
  return $ \env -> do
     val <- exp emptyEnv
     return val
evalImplPhrase tree = evalNotSupported tree


evalDefinition :: Definition -> Eval ProgramFn
evalDefinition (DefLet (PatIdent name) expr) = do
  exp <- evalExpression expr
  return $ \env -> do
     val <- exp emptyEnv
     modify $ \e -> setVariable e name val
     return val
evalDefinition (DefLetFun (PatIdent name) (fnParams) expr) = do
  exp <- evalExpression expr
  return $ \env -> do
     fnBody <- return $ \params env -> do
        val <- modify (\s -> setPatterns s fnParams params) >> (exp emptyEnv)
        return val
     let fn = RFunc (RFuncSignature (map (\_ -> TUnknown) fnParams) TUnknown) (RFuncBody fnBody) (name : (map patternToIdent fnParams)) in (do
       modify (\e -> setVariable e name fn)
       return fn)
evalDefinition tree = evalNotSupported tree


evalExpression :: Expression -> Eval ProgramFn
evalExpression (EIdent name) = do
    return $ \env -> do
        state <- get
        return $ getVariable state name
evalExpression (EConst (CInt val)) = do
    return $ \env -> do
        return (RInt val)
evalExpression (EConst (CBool CBTrue)) = do
    return $ \env -> do
        return (RBool True)
evalExpression (EConst (CBool CBFalse)) = do
    return $ \env -> do
        return (RBool False)
evalExpression (EConst (CString val)) = do
    return $ \env -> do
        return (RString val)
evalExpression (EParens expr) = do
    exp <- evalExpression expr
    return $ \env -> do
       val <- exp emptyEnv
       return val
evalExpression (EComplex (ENIf cond th el)) = do
    expCondition <- evalExpression cond
    expThen <- evalExpression th
    expElse <- evalExpression el
    return $ \env -> do
      condVal <- expCondition emptyEnv
      boolCond <- case condVal of
        RBool val -> return val
        v -> throwError $ "TypeError: Cannot evaluate if condition on " ++ (getTypeString v)
      result <- (if boolCond then (expThen emptyEnv) else (expElse emptyEnv))
      return result
evalExpression (EComplex (ENLet pat patExpr e)) = do
    exprInner <- evalExpression e
    exprPattern <- evalExpression patExpr
    return $ \env -> do
      valPattern <- exprPattern emptyEnv
      result <- modify (\s -> setPattern s pat valPattern) >> (exprInner emptyEnv)
      return result
evalExpression (EComplex (ENFun (fnParams) expr)) = do
    exp <- evalExpression expr
    return $ \env -> do
       fnBody <- return $ \params env -> do
          val <- modify (\s -> setPatterns s fnParams params) >> (exp emptyEnv)
          return val
       let fn = RFunc (RFuncSignature (map (\_ -> TUnknown) fnParams) TUnknown) (RFuncBody fnBody) [] in (do
         return fn)
evalExpression (EComplex (ENCall name args)) = do
    argsExprs <- (mapM (\arg -> evalExpression arg) args)
    return $ \env -> do
      state <- get
      fn <- return $ getVariable state name
      fnBody <- case fn of
        RFunc _ (RFuncBody body) _ -> return body
        RInvalid -> throwError $ "CallError: Called object does not exist"
        val -> return (\_ _ -> return val)
      expVals <- mapM (\exp -> exp emptyEnv) argsExprs
      result <- fnBody expVals emptyEnv
      return result
evalExpression (EComplex (ENInfix exprA op exprB)) = do
    expA <- evalExpression exprA
    expB <- evalExpression exprB
    return $ \env -> do
       valA <- expA emptyEnv
       valB <- expB emptyEnv
       res <- callInfixOperator op valA valB
       return res
evalExpression tree = evalNotSupported tree

----------------

genHS :: Implementation -> Bool -> Eval String
genHS INothing _ = do
  return ""
genHS (IRootComplex a b) shouldPrintAST = do
  expA <- genHS a shouldPrintAST
  expB <- genHS b shouldPrintAST
  return (expA ++ expB)
genHS (IRoot a) shouldPrintAST = do
  exp <- genHSImplPhrase a shouldPrintAST
  return exp

genHSImplPhrase :: ImplPhrase -> Bool -> Eval String
genHSImplPhrase (IPhrase expr) shouldPrintAST = do
  return (if shouldPrintAST then (treeToStr expr) else "")
genHSImplPhrase (IDef expr) shouldPrintAST = do
  return (if shouldPrintAST then (treeToStr expr) else "")
genHSImplPhrase (IMacro macro) shouldPrintAST = genHSMacro macro shouldPrintAST

genHSMacro :: BuiltinMacro -> Bool -> Eval String
genHSMacro (MacHSSuite impl code) _ = do
  x <- return $ putStrLn "Parse suite macro"
  contents <- genHS impl True
  header <- return $ [r|
    -- This code was automatically generated by Ocaml.hs macro generator --

    {-# LANGUAGE QuasiQuotes #-}
    module MainSpec (spec) where

    import Text.RawString.QQ

    import Test.Hspec
    import Environment
    import Runtime
    import Lib

    spec :: Spec
    spec = do
      describe "Interpreter arithmetics" $ do
        it "Should add two numbers" $ do
          res <- run 2 |] ++ "([r|" ++ contents ++ "|])" ++
          (foldl (\acc (CString str) -> acc ++ [r|
          |] ++ str) "" code)
  return $ unlines [ header ]

runGenHSAST tree env = runExceptT (runReaderT (runStateT (genHS tree False) (EmptyState)) (env))
genHSAST tree env = do
  e <- runGenHSAST tree env
  output <- return (case e of
    Left s -> s
    Right (e, _) -> e)
  return output
-------------


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

execFunction :: RFuncBody -> [RuntimeValue] -> Environment -> IO ProgramResult
execFunction (RFuncBody body) args env = do
  e <- runExceptT (runReaderT (runStateT (body args env) (env)) (env))
  result <- return (case e of
    Left err -> FailedExecution err
    Right (res, state) -> Executed res state)
  return result

runAST :: Implementation -> Environment -> IO ProgramResult
runAST tree env = do
  program <- interpretAST (removeMacros tree) env
  result <- (case program of
    Valid p -> execProgram p env
    Invalid s -> return (FailedParse s))
  return result