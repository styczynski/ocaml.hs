{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Syntax
import Env
import Type
import AbsSyntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import System.IO.Unsafe

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a = (ReaderT
                  Env             -- Typing environment
                  (StateT         -- Inference state
                  InferState
                  (Except         -- Inference errors
                    TypeError))
                  a)              -- Result

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid, Semigroup)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply s (t1 `TTuple` t2) = apply s t1 `TTuple` apply s t2
  apply s (TList a) = TList $ apply s a
  apply s TUnit = TUnit

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TList a)      = ftv a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TTuple` t2) = ftv t1 `Set.union` ftv t2
  ftv TUnit = Set.empty

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Ident
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | Debug String
  deriving (Show)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer 

-- | Solve for the toplevel type of an expression in a given environment
inferAST :: Env -> Implementation -> Either TypeError Scheme
inferAST env ex = case runInfer env (inferImplementation ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.empty

-- | Extend type environment
inEnv :: (Ident, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = (remove e x) `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Ident -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    ->  do t <- instantiate s
                       return t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Infer Type
ops Add = return $ typeInt `TArr` (typeInt `TArr` typeInt)
ops Mul = return $ typeInt `TArr` (typeInt `TArr` typeInt)
ops Sub = return $ typeInt `TArr` (typeInt `TArr` typeInt)
ops Eql = return $ typeInt `TArr` (typeInt `TArr` typeBool)
ops OpCons = do
  tv <- fresh
  return $ (tv) `TArr` ((TList tv) `TArr` (TList tv) )
ops OpTupleCons = do
  tv <- fresh
  tv2 <- fresh
  tv3 <- fresh
  return $ (tv) `TArr` ((TTuple tv2 tv3) `TArr` (TTuple tv (TTuple tv2 tv3)))

opsUni :: Uniop -> Infer Type
opsUni OpHead = do
  tv <- fresh
  return $ (TList tv) `TArr` (tv)
opsUni OpTails = do
  tv <- fresh
  return $ (TList tv) `TArr` (TList tv)
opsUni OpEmptyList = do
  tv <- fresh
  tv2 <- fresh
  return $ tv `TArr` (TList tv2)
opsUni OpEmptyTuple = do
  tv <- fresh
  return $ tv `TArr` (TTuple TUnit TUnit)
opsUni (OpTupleNth index len) = do
  (tupleType, elsTypes) <- foldrM (\_ (tup, tvs) -> do
    tv <- fresh
    return $ ((TTuple tv tup), [tv] ++ tvs)) ((TTuple TUnit TUnit), []) (replicate len 0)
  return $ (tupleType) `TArr` (elsTypes !! index)

withTypeAnnot :: TypeConstraint -> Expr -> Infer Expr
withTypeAnnot TypeConstrEmpty e = return e
withTypeAnnot (TypeConstrDef texpr) e = do
  s <- resolveTypeExpression texpr
  return $ Check e s

inferImplementation :: Implementation -> Infer (Type, [Constraint])
inferImplementation (IRoot [phr0]) = inferPhrase phr0

getTypeExpressionFV :: TypeExpression -> Set.Set String
getTypeExpressionFV (TypeExprAbstract (TypeIdentAbstract name)) = Set.singleton name
getTypeExpressionFV (TypeExprList listType) = getTypeExpressionFV listType
getTypeExpressionFV (TypeFun a b) = (getTypeExpressionFV a) `Set.union` (getTypeExpressionFV b)
getTypeExpressionFV (TypeExprTuple fstEl restEls) =
  foldr (\el acc -> acc `Set.union` (getTypeExpressionFV el)) Set.empty ([fstEl] ++ restEls)
getTypeExpressionFV _ = Set.empty

resolveTypeExpressionRec :: (Set.Set String) -> TypeExpression -> Infer Type
resolveTypeExpressionRec fvs (TypeExprIdent (Ident name)) = return $ TCon name
resolveTypeExpressionRec fvs (TypeExprList expr) = do
  t <- resolveTypeExpressionRec fvs expr
  return $ TList t
resolveTypeExpressionRec fvs (TypeFun a b) = do
  t1 <- resolveTypeExpressionRec fvs a
  t2 <- resolveTypeExpressionRec fvs b
  return $ TArr t1 t2
resolveTypeExpressionRec fvs (TypeExprTuple fstEl restEls) = do
  tupleT <- foldrM (\expr acc -> do
    t <- resolveTypeExpressionRec fvs expr
    return $ TTuple t acc) (TTuple TUnit TUnit) ([fstEl] ++ restEls)
  return tupleT
resolveTypeExpressionRec fvs (TypeExprAbstract (TypeIdentAbstract name)) = do
  parsedName <- return $ [ x | x <- name, not (x `elem` "'") ]
  validFvs <- return $ name `Set.member` fvs
  tv <- fresh
  if not validFvs then
    throwError $ Debug $ "Type name " ++ name ++ " is not a valid polymorhic type name"
  else
    return $ tv

getConstScheme :: Constant -> Scheme
getConstScheme (CInt _) = Forall [] (TCon "Int")
getConstScheme (CBool _) = Forall [] (TCon "Bool")
getConstScheme (CString _) = Forall [] (TCon "String")

resolveTypeExpression :: TypeExpression -> Infer Scheme
resolveTypeExpression exp = do
  fvs <- return $ getTypeExpressionFV exp
  t <- resolveTypeExpressionRec fvs exp
  fvsT <- return $ map (\e -> TV e) $ Set.elems fvs
  return $ Forall fvsT t

simplifyPhrase :: ImplPhrase -> Infer Expr
simplifyPhrase (IPhrase expr) = simplifyComplexExpression expr

simplifyPattern :: SimplePattern -> Expr -> Expr -> Infer Expr
simplifyPattern PatNone _ expr = return expr
simplifyPattern (PatConst const) letExpr expr =
  return $ Let (Ident "x") (Check letExpr $ getConstScheme const) expr
simplifyPattern (PatIdent name) letExpr expr =
  return $ Let name letExpr expr
simplifyPattern (PatCons hPat tPat) letExpr expr = do
  hSimpl <- simplifyPattern hPat (UniOp OpHead letExpr) expr
  tSimpl <- simplifyPattern tPat (UniOp OpTails letExpr) hSimpl
  return tSimpl
simplifyPattern (PatTuple (PTuple el restEls)) letExpr expr = do
  tupleCount <- return $ 1 + length restEls
  (tSimpl, _) <- foldrM (\(PTupleElement el) (expr, k) -> do
    pSimpl <- simplifyPattern el (UniOp (OpTupleNth k tupleCount) letExpr) expr
    return (pSimpl, k+1)) (expr, 0) ([el] ++ restEls)
  return tSimpl

simplifyComplexExpression :: ComplexExpression -> Infer Expr
simplifyComplexExpression (ECTuple tuple) = simplifyTuple tuple
simplifyComplexExpression (ECExpr expr) = simplifyExpression expr

simplifyComplexExpression (ECMatch expr _ clauses) = do
  clausesList <- foldrM (\(MatchClause pat clauseExpr) acc -> do
    r <- return $ ListElement $ ECLet LetRecNo pat [] TypeConstrEmpty expr clauseExpr
    return $ [r] ++ acc) [] clauses
  r <- simplifyComplexExpression $ ECExpr $ ExprList $ DList clausesList
  return $ UniOp OpHead r

simplifyComplexExpression (ECFunction bPip matchClauses) = do
  simplifyComplexExpression $ ECFun (PatIdent (Ident "x")) [] $ ECMatch (ECExpr $ ExprVar (Ident "x")) bPip matchClauses
simplifyComplexExpression (ECFun pat argsPat expr) = do
  simplifyComplexExpression (ECLet LetRecNo (PatIdent $ Ident "x") ([pat] ++ argsPat) TypeConstrEmpty expr (ECExpr $ ExprVar $ Ident "x"))
simplifyComplexExpression (ECLet _ pat [] typeAnnot letExpr expr) = do
  letSimpl <- simplifyComplexExpression letExpr
  exprSimpl <- simplifyComplexExpression expr
  r <- simplifyPattern pat letSimpl exprSimpl
  r <- withTypeAnnot typeAnnot r
  return r
simplifyComplexExpression (ECLet _ pat argsPats typeAnnot letExpr expr) = do
  letSimpl <- simplifyComplexExpression letExpr
  exprSimpl <- simplifyComplexExpression expr
  letSimplAcc <- foldrM (\pat expr -> do
    s <- simplifyPattern pat (Var $ Ident "x") expr
    return $ Lam (Ident "x") s) letSimpl argsPats
  r <- simplifyPattern pat letSimplAcc exprSimpl
  r <- withTypeAnnot typeAnnot r
  return r

simplifyTuple :: DTuple -> Infer Expr
simplifyTuple (DTuple firstElem elems) = do
  elemsSimpl <- foldM (\expr (DTupleElement el) -> do
    k <- simplifyExpression el
    return $ Op OpTupleCons k expr) (UniOp OpEmptyTuple Skip) ([firstElem] ++ elems)
  return $ elemsSimpl

simplifyList :: DList -> Infer Expr
simplifyList (DList elems) = do
  elemsSimpl <- foldM (\expr (ListElement el) -> do
    k <- simplifyComplexExpression el
    return $ Op OpCons k expr) (UniOp OpEmptyList Skip) elems
  return $ elemsSimpl

simplifyExpression :: Expression -> Infer Expr
simplifyExpression (ExprVar name) = return $ Var name
simplifyExpression (ExprConst (CInt val)) = return $ Lit $ LInt val
simplifyExpression (ExprConst (CBool CBTrue)) = return $ Lit $ LBool True
simplifyExpression (ExprConst (CBool CBFalse)) = return $ Lit $ LBool False
simplifyExpression (ExprList list) = simplifyList list
simplifyExpression (ExprCompl expr) = simplifyComplexExpression expr


inferPhrase :: ImplPhrase -> Infer (Type, [Constraint])
inferPhrase ast = do
  tree <- simplifyPhrase ast
  --throwError $ Debug $ (show tree)
  infer tree

infer :: Expr -> Infer (Type, [Constraint])
infer expr = case expr of
  Skip -> return (typeInt, [])
  Lit (LInt _)  -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])

  Check e (Forall _ t) -> do
    (t1, c1) <- infer e
    return (t1, c1 ++ [(t1, t)])

  Var x -> do
      t <- lookupEnv x
      return (t, [])

  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] tv) (infer e)
    return (tv `TArr` t, c)

  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])

  Let x e1 e2 -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (apply sub env) (apply sub t1)
            (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
            return (t2, c1 ++ c2)

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 ++ [(tv `TArr` tv, t1)])

  UniOp op e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    u1 <- return $ t1 `TArr` tv
    u2 <- opsUni op
    return (tv, c1 ++ [(u1, u2)])

  Op op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    u1 <- return $ t1 `TArr` (t2 `TArr` tv)
    u2 <- ops op
    return (tv, c1 ++ c2 ++ [(u1, u2)])

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, typeBool), (t2, t3)])

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TList a)  = fv a
    fv (TTuple a b) = fv a ++ fv b
    fv TUnit = []
    fv (TCon _)   = []

    normtype TUnit = TUnit
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TTuple a b) = TTuple (normtype a) (normtype b)
    normtype (TList a) = TList (normtype a)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TList t1) (TList t2) = unifyMany [t1] [t2]
unifies (TTuple t1 t2) (TTuple t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

