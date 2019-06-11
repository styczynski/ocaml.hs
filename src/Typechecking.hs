module Typechecking where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import AbsSyntax
import Runtime
import Environment
import TypecheckingUtils
import TypecheckingPatterns

import qualified Data.Map as Map
import qualified Data.Set as Set

tiFuncCurry :: TypeEnv -> (TypeEnv -> TI (Subst, RuntimeType)) -> String -> TI (Subst, RuntimeType)
tiFuncCurry env exprFn n = do
  tv <- newTyVar "a"
  (TypeEnv env0) <- return $ remove env n
  env00 <- return $ TypeEnv (env0 `Map.union` (Map.singleton n (Scheme [ ] tv)))
  (s1, t1) <- exprFn env00
  return (s1, TFunEx (apply s1 tv) t1)

tiFunc :: TypeEnv -> ComplexExpression -> [String] -> TI (Subst, RuntimeType)
tiFunc env expr [] =
  tiComplexExpression env expr
tiFunc env expr (h:t) =
  tiFuncCurry env (\e -> tiFunc e expr t) h

tiImpl :: TypeEnv -> Implementation -> TI (Subst, RuntimeType)
tiImpl env (IRoot (h:_)) = do
  tiPhrase env h

tiPhrase :: TypeEnv -> ImplPhrase -> TI (Subst, RuntimeType)
tiPhrase env (IPhrase expr) = do
  tiComplexExpression env expr

tiComplexExpression :: TypeEnv -> ComplexExpression -> TI (Subst, RuntimeType)
tiComplexExpression env (ECExpr expr) = do
  tiExpression env expr
tiComplexExpression env (ECLet recKeyword pat [] _ letExpr expr) = do
  (s1, e1) <- (tiComplexExpression env letExpr) >>= tiPattern env pat
  (sr, tr) <- tiComplexExpression e1 expr
  return (s1 `composeSubst` sr, tr)
tiComplexExpression env (ECLet recKeyword (PatIdent (Ident name)) [(PatIdent (Ident arg0))] _ letExpr expr) = do
  (s1, t1) <- tiFunc env letExpr [arg0]
  (TypeEnv env2) <- return $ remove env name
  t2 <- return $ generalize (apply s1 env) t1
  env3 <- return $ TypeEnv (Map.insert name t2 env2)
  (sr, tr) <- tiComplexExpression (apply s1 env3) expr
  return (s1 `composeSubst` sr, tr)

tiSimpleExpression :: TypeEnv -> SimpleExpression -> TI (Subst, RuntimeType)
tiSimpleExpression env (ESConst const) = tiLit env const
tiSimpleExpression env (ESList list) = tiList env list
tiSimpleExpression env (ESExpr expr) = tiComplexExpression env expr
--tiSimpleExpression env (ESIdent name) = tiExpression env (ExprCall name [])

tiExpression :: TypeEnv -> Expression -> TI (Subst, RuntimeType)
tiExpression env (ExprConst const) = tiLit env const
tiExpression env (ExprList list) = tiList env list
--tiExpression (TypeEnv env) (ExprCall (Ident name) []) =
--  case Map.lookup name env of
--    Nothing -> throwError $ "unbound variable: " ++ name
--    Just sigma -> do
--      t <- instantiate sigma
--      return (nullSubst, t)

tiList :: TypeEnv -> DList -> TI (Subst, RuntimeType)
tiList env (DList elems) = do
  exprs <- return $ map (\(ListElement e) -> e) elems
  exprsTi <- mapM (\expr -> tiComplexExpression env expr) exprs
  case exprsTi of
    ((s,t):_) -> return (s,TList t)
    _ -> do
      tv <- newTyVar "a"
      return (nullSubst, TList tv)

tiLit :: TypeEnv -> Constant -> TI (Subst, RuntimeType)
tiLit _ (CInt _) = return (nullSubst, TInt)
tiLit _ (CString _) = return (nullSubst, TString)
tiLit _ (CBool _) = return (nullSubst, TBool)

typeInference :: Map.Map String Scheme -> Implementation -> TI RuntimeType
typeInference env e = do
  (s, t) <- tiImpl (TypeEnv env) e
  return $ apply s t

runTypeInference :: Implementation -> IO ()
runTypeInference e = do
  (res, _) <- runTI (typeInference Map.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err
    Right t -> putStrLn $ typeToStr t