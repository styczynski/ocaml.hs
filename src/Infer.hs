{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Syntax
import Env
import Type
import AbsSyntax
import PrintSyntax

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

type Infer = StateT (InferState) (ReaderT (Env) (ExceptT TypeError IO))

-- | Inference state
data InferState = InferState { count :: Int, inferTrace :: [String], lastInferExpr :: String }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0, inferTrace = [], lastInferExpr = "" }

type Constraint = (Type, Type)

type Unifier = (Subst, [AConstraint])

-- | Solve state
data SolveState = SolveState { lastAnnot :: TypeErrorPayload }
initSolve :: SolveState
initSolve = SolveState { lastAnnot = EmptyPayload }

-- | Constraint solver monad
type Solve a = (StateT
                 SolveState
                 (Except
                   TypeError))
                 a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Show, Monoid, Semigroup)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s (TDep name deps) = TDep name $ map (\a -> apply s a) deps
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  apply s (t1 `TTuple` t2) = apply s t1 `TTuple` apply s t2
  apply s (TList a) = TList $ apply s a
  apply s TUnit = TUnit
  apply s (TExport v) = (TExport v)

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TList a)      = ftv a
  ftv (TDep name deps) = foldl (\acc el -> acc `Set.union` (ftv el)) (Set.empty) deps
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TTuple` t2) = ftv t1 `Set.union` ftv t2
  ftv TUnit = Set.empty
  ftv (TExport _) = Set.empty

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable AConstraint where
   apply s (AConstraint l (t1, t2)) = AConstraint l (apply s t1, apply s t2)
   ftv (AConstraint _ (t1, t2)) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)

data TypeError
  = UnificationFail TypeErrorPayload Type Type
  | InfiniteType TypeErrorPayload TVar Type
  | UnboundVariable TypeErrorPayload Ident
  | Ambigious TypeErrorPayload [Constraint]
  | UnificationMismatch TypeErrorPayload [Type] [Type]
  | Debug TypeErrorPayload String
  deriving (Show)

constraintToStr :: Constraint -> String
constraintToStr (a,b) = (typeToStr [] a) ++ " ~ " ++ (typeToStr [] b)

constraintsListToStr :: [Constraint] -> String
constraintsListToStr l = "{" ++ (foldr (\t acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ (constraintToStr t)) "" l) ++ "}"

typesListToStr :: [Type] -> String
typesListToStr l = "{" ++ (foldr (\t acc -> acc ++ (if (length acc) <= 0 then "" else ", ") ++ (typeToStr [] t)) "" l) ++ "}"

--data TypeErrorPayload = EmptyPayload | TypeErrorPayload String deriving (Show)
generateTypePayloadMessage :: TypeErrorPayload -> String
generateTypePayloadMessage EmptyPayload = "Typechecking error:\nLocation: <unknown>\n\n"
generateTypePayloadMessage (TypeErrorPayload ast) = "Typechecking error:\nLocation: " ++ ast ++ "\n\n"

typeErrorToStr :: TypeError -> String
typeErrorToStr (UnificationFail payl a b) = (generateTypePayloadMessage payl) ++ "Cannot match types, expected: " ++ (typeToStr [] b) ++ ", got: " ++ (typeToStr [] a)
typeErrorToStr (Debug payl mes) = (generateTypePayloadMessage payl) ++ mes
typeErrorToStr (UnificationMismatch payl a b) = (generateTypePayloadMessage payl) ++ "Cannot match types, mismatch when unyfying: " ++ (typesListToStr a) ++ " and " ++ (typesListToStr b)
typeErrorToStr (Ambigious payl a) = (generateTypePayloadMessage payl) ++ "Cannot infer types, expression is ambigious: " ++ (constraintsListToStr a)
typeErrorToStr (UnboundVariable payl (Ident a)) = (generateTypePayloadMessage payl) ++ "Variable not in scope: \"" ++ a ++ "\""
typeErrorToStr (InfiniteType payl (TV v) t) = (generateTypePayloadMessage payl) ++ "Infinite type detected: " ++ v ++ "': " ++ (typeToStr [] t)
typeErrorToStr e = (generateTypePayloadMessage EmptyPayload) ++ "Got unexpected error during type inference phase.\n" ++ (show e)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> InferState -> Infer (Env, Type, [AConstraint]) -> IO (Either TypeError ((Env, Type, [AConstraint]), InferState))
runInfer env state fn = do
  --let v = runExcept $ runStateT (runReaderT m env) state in
  v <- runExceptT (runReaderT (runStateT fn (state)) (env))
  case v of
    (Left e) -> return $ Left e
    (Right ((env, t, c), state)) -> return $ Right ((env, t, c), state)

solve :: Either TypeError (Type, [AConstraint]) -> Either TypeError Scheme
solve r = case r of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

unpackEnvTypeContraints :: Either TypeError ((Env, Type, [AConstraint]), InferState) -> Either TypeError (Type, [AConstraint])
unpackEnvTypeContraints (Left r) = Left r
unpackEnvTypeContraints (Right ((_, t, c),_)) = Right (t, c)

retrieveEnv :: Either TypeError ((Env, Type, [AConstraint]), InferState) -> Env
retrieveEnv (Left r) = empty
retrieveEnv (Right ((e,_,_),_)) = e

retrieveState :: Either TypeError ((Env, Type, [AConstraint]), InferState) -> InferState
retrieveState (Left r) = initInfer
retrieveState (Right (_,state)) = state

-- | Solve for the toplevel type of an expression in a given environment
inferAST :: Env -> InferState -> Implementation -> IO (Either TypeError (Scheme, Env, InferState))
inferAST env state ex = do
  i <- runInfer env state (inferImplementation ex)
  env <- return $ retrieveEnv i
  state <- return $ retrieveState i
  scheme <- return $ solve $ unpackEnvTypeContraints i
  case scheme of
    Left e -> return $ Left e
    Right s -> return $ Right (s, env, state)

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> InferState -> Expr -> IO (Either TypeError ([AConstraint], Subst, Type, Scheme))
constraintsExpr env state ex = do
  r <- runInfer env state (inferE ex)
  case r of
    Left err -> return $ Left err
    Right ((_, ty, cs), state) -> case runSolve cs of
      Left err -> return $ Left err
      Right subst -> return $ Right (cs, subst, ty, sc)
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
      Nothing   ->  do payl <- errPayload
                       throwError $ UnboundVariable payl x
      Just s    ->  do t <- instantiate s
                       return t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

addExprAnnot :: Infer Expr -> Infer Expr
addExprAnnot inExpr = do
  s <- get
  inferTraceTop <- return $ let InferState { inferTrace = inferTrace } = s in let (h:_) = inferTrace in h
  e <- inExpr
  return $ Annot inferTraceTop e

markTrace :: (Show a, Print a) => a -> Infer ()
markTrace a = do
  s <- get
  inferTrace <- return $ let InferState { inferTrace = inferTrace } = s in inferTrace
  put s{ inferTrace = ([(printTree a)] ++ inferTrace) }
  return ()

unmarkTrace :: (Show a, Print a) => a -> Infer ()
unmarkTrace a = do
  s <- get
  newTrace <- return $ let InferState { inferTrace = inferTrace } = s in drop 1 inferTrace
  put s{ inferTrace = newTrace }
  return ()

errPayload :: Infer TypeErrorPayload
errPayload = do
  s <- get
  lastTraceStr <- return $ let InferState { lastInferExpr = lastInferExpr } = s in lastInferExpr
  return $ TypeErrorPayload lastTraceStr

freshIdent :: Infer Ident
freshIdent = do
    s <- get
    put s{count = count s + 1}
    return $ Ident $ "__@$internal_variable__" ++ (letters !! count s) ++ "_"

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
ops OpSemicolon = do
  tv1 <- fresh
  tv2 <- fresh
  return $ tv1 `TArr` (tv2 `TArr` tv2)
ops OpSame = do
  tv <- fresh
  return $ tv `TArr` (tv `TArr` tv)
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
opsUni OpListNth = do
  tv <- fresh
  return $ (TList tv) `TArr` tv
opsUni (OpTupleNth index len) = do
  (tupleType, elsTypes) <- foldrM (\_ (tup, tvs) -> do
    tv <- fresh
    return $ ((TTuple tv tup), [tv] ++ tvs)) ((TTuple TUnit TUnit), []) (replicate len 0)
  return $ (tupleType) `TArr` (elsTypes !! index)

withTypeAnnot :: TypeConstraint -> ComplexExpression -> Infer ComplexExpression
withTypeAnnot TypeConstrEmpty e = return e
withTypeAnnot (TypeConstrDef texpr) e = do
  return $ ECChecked e texpr

isRec :: LetRecKeyword -> Bool
isRec LetRecYes = True
isRec LetRecNo = False

inferImplementation :: Implementation -> Infer (Env, Type, [AConstraint])
inferImplementation ast@(IRoot cores) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) core -> do
    i <- local (\_ -> envAcc) $ inferImplementationCore core
    return i) (env, TUnit, []) cores
  unmarkTrace ast
  return r

inferImplementationCore :: ImplementationCore -> Infer (Env, Type, [AConstraint])
inferImplementationCore ast@(IRootExpr expr) = do
  markTrace ast
  env <- ask
  (t, c) <- inferComplexExpression expr
  unmarkTrace ast
  return (env, t, c)
inferImplementationCore ast@(IRootDef phrases) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) phrase -> do
    i <- local (\_ -> envAcc) $ inferImplementationPhrase phrase
    return i) (env, TUnit, []) phrases
  unmarkTrace ast
  return r

createTypeExpressionAbstractArgConstructor :: Ident -> [String] -> TypeExpression
createTypeExpressionAbstractArgConstructor typeName [] =
  TypeExprSimple $ TypeSExprIdent $ typeName
createTypeExpressionAbstractArgConstructor typeName names@(hNames:tNames) =
  let (identHead:identTail) = map (\e -> TypeArgEl $ TypeExprSimple $ TypeSExprAbstract $ TypeIdentAbstract $ e) names in
   TypeExprIdent (TypeArgJust identHead identTail) typeName

inferVariantOption :: [String] -> Ident -> TDefVariant -> Infer (Env, Type, [AConstraint])
inferVariantOption typeVars typeName (TDefVarSimpl name@(Ident nameStr)) = do
  retType <- return $ createTypeExpressionAbstractArgConstructor typeName typeVars
  reverseType <- return $ TypeFun retType retType
  (r0, _, _) <- inferImplementationPhrase $ IGlobalLet LetRecNo (PatIdent $ Ident $ nameStr ++ "_reverse") [] TypeConstrEmpty $ ECTyped reverseType
  r <- local (\_ -> r0) $ inferImplementationPhrase $ IGlobalLet LetRecNo (PatIdent name) [] TypeConstrEmpty $ ECTyped retType
  return r
inferVariantOption typeVars typeName (TDefVarCompl name@(Ident nameStr) typeExpr) = do
  fvsNames <- resolveTypeExpressionFVNames typeExpr
  payl <- errPayload
  _ <- if fvsNames `Set.isSubsetOf` (Set.fromList typeVars) then return 0 else throwError $ Debug payl $ "Invalid abstract variable used in type definition."
  -- [(PatCheck (Ident "x") typeExpr)]
  retType <- return $ createTypeExpressionAbstractArgConstructor typeName typeVars
  selType <- return $ TypeFun (typeExpr) retType
  selReverseType <- return $ TypeFun retType (typeExpr)
  (r0, _, _) <- inferImplementationPhrase $ IGlobalLet LetRecNo (PatIdent $ Ident $ nameStr ++ "_reverse") [] TypeConstrEmpty $ ECTyped selReverseType
  r <- local (\_ -> r0) $ inferImplementationPhrase $ IGlobalLet LetRecNo (PatIdent name) [] TypeConstrEmpty $ ECTyped selType
  return r

typeParamsToList :: TypeParam -> [String]
typeParamsToList TypeParamNone = []
typeParamsToList (TypeParamJust names) = map (\(TypeIdentAbstract name) -> name) names
typeParamsToList (TypeParamJustOne (TypeIdentAbstract name)) = [name]

inferTypeDef :: TypeDef -> Infer (Env, Type, [AConstraint])
inferTypeDef ast@(TypeDefVar typeParams name options) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) option -> do
    i <- local (\_ -> envAcc) $ inferVariantOption (typeParamsToList typeParams) name option
    return i) (env, TUnit, []) options
  unmarkTrace ast
  return r
inferTypeDef ast@(TypeDefVarP typeParams name options) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) option -> do
    i <- local (\_ -> envAcc) $ inferVariantOption (typeParamsToList typeParams) name option
    return i) (env, TUnit, []) options
  unmarkTrace ast
  return r

inferImplementationPhrase :: ImplPhrase -> Infer (Env, Type, [AConstraint])
inferImplementationPhrase (IGlobalLetOperator recK opName restPatterns letExpr) = do
  (t, c) <- inferComplexExpression (ECLetOperator recK opName restPatterns letExpr $ ECExportEnv)
  return $ let (TExport exportedEnv) = t in (exportedEnv, t, c)
inferImplementationPhrase (IGlobalLet recK pattern restPatterns typeAnnot letExpr) = do
  (t, c) <- inferComplexExpression (ECLet recK pattern restPatterns typeAnnot letExpr $ ECExportEnv)
  return $ let (TExport exportedEnv) = t in (exportedEnv, t, c)
inferImplementationPhrase (IDefType typeDef) = inferTypeDef typeDef

getTypeSimpleExpressionFV :: TypeSimpleExpression -> Infer (Map.Map String TVar)
getTypeSimpleExpressionFV (TypeSExprList listType) = getTypeExpressionFV listType
getTypeSimpleExpressionFV (TypeSExprIdent _) = return Map.empty
getTypeSimpleExpressionFV TypeSExprEmpty = return Map.empty
getTypeSimpleExpressionFV (TypeSExprAbstract (TypeIdentAbstract name)) = do
  tvv <- fresh
  return $ let (TVar tv) = tvv in Map.singleton name tv

getTypeExpressionFV :: TypeExpression -> Infer (Map.Map String TVar)
getTypeExpressionFV  (TypeExprSimple simpl) = getTypeSimpleExpressionFV simpl
getTypeExpressionFV (TypeExprIdent (TypeArgJustOne param) _) = getTypeSimpleExpressionFV param
getTypeExpressionFV (TypeExprIdent (TypeArgJust firstParam restParams) _) = do
  foldlM (\acc (TypeArgEl el) -> do
    r <- getTypeExpressionFV el
    return $ acc `Map.union` r) Map.empty ([firstParam] ++ restParams)
getTypeExpressionFV (TypeFun a b) = do
  t1 <- (getTypeExpressionFV a)
  t2 <- (getTypeExpressionFV b)
  return $ t1 `Map.union` t2
getTypeExpressionFV (TypeExprTuple fstEl restEls) =
  foldlM (\acc el -> do
    t <- (getTypeExpressionFV el)
    return $ acc `Map.union` t) Map.empty ([fstEl] ++ restEls)
getTypeExpressionFV _ = return Map.empty

resolveTypeSimpleExpressionFVNames :: TypeSimpleExpression -> Infer (Set.Set String)
resolveTypeSimpleExpressionFVNames TypeSExprEmpty = return $ Set.empty
resolveTypeSimpleExpressionFVNames (TypeSExprList expr) = resolveTypeExpressionFVNames expr
resolveTypeSimpleExpressionFVNames (TypeSExprAbstract (TypeIdentAbstract name)) = return $ Set.singleton name
resolveTypeSimpleExpressionFVNames (TypeSExprIdent _) = return $ Set.empty

resolveTypeExpressionFVNames :: TypeExpression -> Infer (Set.Set String)
resolveTypeExpressionFVNames (TypeExprSimple simpl) = resolveTypeSimpleExpressionFVNames simpl
resolveTypeExpressionFVNames (TypeExprIdent (TypeArgJustOne simpl) _) = resolveTypeSimpleExpressionFVNames simpl
resolveTypeExpressionFVNames (TypeExprIdent (TypeArgJust firstParam restParams) _) = do
  foldlM (\acc (TypeArgEl el) -> do
    r <- resolveTypeExpressionFVNames el
    return $ acc `Set.union` r) Set.empty ([firstParam] ++ restParams)
resolveTypeExpressionFVNames (TypeFun a b) = do
  x <- resolveTypeExpressionFVNames a
  y <- resolveTypeExpressionFVNames b
  return $ x `Set.union` y
resolveTypeExpressionFVNames (TypeExprTuple firstElem restElems) = do
  x <- resolveTypeExpressionFVNames firstElem
  foldrM (\el acc -> do
    y <- resolveTypeExpressionFVNames el
    return $ acc `Set.union` y) x restElems

resolveTypeSimpleExpressionRec :: (Map.Map String TVar) -> TypeSimpleExpression -> Infer Type
resolveTypeSimpleExpressionRec fvs TypeSExprEmpty = return TUnit
resolveTypeSimpleExpressionRec fvs (TypeSExprIdent (Ident name)) = return $ TCon name
resolveTypeSimpleExpressionRec fvs (TypeSExprList expr) = do
  t <- resolveTypeExpressionRec fvs expr
  return $ TList t
resolveTypeSimpleExpressionRec fvs (TypeSExprAbstract (TypeIdentAbstract name)) = do
  parsedName <- return $ [ x | x <- name, not (x `elem` "'") ]
  validFvs <- return $ name `Map.member` fvs
  if not validFvs then do
    payl <- errPayload
    throwError $ Debug payl $ "Type name " ++ name ++ " is not a valid polymorhic type name"
  else
    let (Just tv) = Map.lookup name fvs in
    return $ TVar tv

resolveTypeExpressionRec :: (Map.Map String TVar) -> TypeExpression -> Infer Type
resolveTypeExpressionRec fvs (TypeExprSimple simpl) = resolveTypeSimpleExpressionRec fvs simpl
resolveTypeExpressionRec fvs (TypeExprIdent (TypeArgJust firstParam restParams) (Ident name)) = do
  typeParams <- foldlM (\acc (TypeArgEl expr)-> do
      t <- resolveTypeExpressionRec fvs expr
      return $ [t] ++ acc) ([]) ([firstParam] ++ restParams)
  return $ TDep name typeParams
resolveTypeExpressionRec fvs (TypeExprIdent (TypeArgJustOne param) (Ident name)) = do
  typeParam <- resolveTypeSimpleExpressionRec fvs param
  return $ TDep name [typeParam]
resolveTypeExpressionRec fvs (TypeFun a b) = do
  t1 <- resolveTypeExpressionRec fvs a
  t2 <- resolveTypeExpressionRec fvs b
  return $ TArr t1 t2
resolveTypeExpressionRec fvs (TypeExprTuple fstEl restEls) = do
  tupleT <- foldlM (\acc expr-> do
    t <- resolveTypeExpressionRec fvs expr
    return $ TTuple t acc) (TTuple TUnit TUnit) ([fstEl] ++ restEls)
  return tupleT

getConstScheme :: Constant -> Scheme
getConstScheme (CInt _) = Forall [] (TCon "Int")
getConstScheme (CBool _) = Forall [] (TCon "Bool")
getConstScheme (CString _) = Forall [] (TCon "String")

getOperatorName :: OperatorAny -> String
getOperatorName (OperatorAnyA (OperatorA name)) = name
getOperatorName (OperatorAnyB (OperatorB name)) = name
getOperatorName (OperatorAnyC (OperatorC name)) = name
getOperatorName (OperatorAnyD (OperatorD name)) = name
getOperatorName (OperatorAnyDS (OperatorDS)) = "*"
getOperatorName (OperatorAnyE (OperatorE name)) = name
getOperatorName (OperatorAnyF (OperatorF name)) = name

resolveTypeExpression :: TypeExpression -> Infer Scheme
resolveTypeExpression exp = do
  fvs <- getTypeExpressionFV exp
  t <- resolveTypeExpressionRec fvs exp
  fvsT <- return $ Map.elems fvs
  return $ Forall fvsT t

simplifyPattern :: Bool -> SimplePattern -> Expr -> Expr -> Infer Expr
simplifyPattern _ PatNone _ expr = addExprAnnot $ return expr
simplifyPattern _ ast@(PatList (PList [])) letExpr expr = do
  markTrace ast
  scheme <- resolveTypeExpression (TypeExprSimple $ TypeSExprList $ TypeExprSimple $ TypeSExprAbstract $ TypeIdentAbstract "'a")
  id <- freshIdent
  unmarkTrace ast
  addExprAnnot $ return $ Let id (Check letExpr scheme) expr
simplifyPattern recMode (PatConstr (Ident nameStr) pat) letExpr expr = do
  addExprAnnot $ simplifyPattern recMode pat (App (Var $ Ident $ nameStr ++ "_reverse") letExpr) expr
simplifyPattern recMode ast@(PatList (PList list)) letExpr expr = do
  markTrace ast
  (simpl, _) <- foldrM (\(PListElement pat) (expr, n) -> do
    patSimpl <- simplifyPattern recMode pat (UniOp OpListNth letExpr) expr
    return (patSimpl, n+1)) (expr, 0) list
  unmarkTrace ast
  addExprAnnot $ return simpl
simplifyPattern _ (PatConst const) letExpr expr = do
  id <- freshIdent
  addExprAnnot $ return $ Let id (Check letExpr $ getConstScheme const) expr
simplifyPattern recMode (PatCheck name typeExpr) letExpr expr = do
  scheme <- resolveTypeExpression typeExpr
  addExprAnnot $ simplifyPattern recMode (PatIdent name) (Check letExpr scheme) expr
simplifyPattern False (PatIdent name) letExpr expr =
  addExprAnnot $ return $ Let name letExpr expr
simplifyPattern True (PatIdent name) letExpr expr =
  addExprAnnot $ return $ Let name (Fix (Lam name letExpr)) expr
simplifyPattern recMode ast@(PatCons hPat tPat) letExpr expr = do
  markTrace ast
  hSimpl <- simplifyPattern recMode hPat (UniOp OpHead letExpr) expr
  tSimpl <- simplifyPattern recMode tPat (UniOp OpTails letExpr) hSimpl
  unmarkTrace ast
  addExprAnnot $ return tSimpl
simplifyPattern recMode ast@(PatTuple (PTuple el restEls)) letExpr expr = do
  markTrace ast
  tupleCount <- return $ 1 + length restEls
  (tSimpl, _) <- foldrM (\(PTupleElement el) (expr, k) -> do
    pSimpl <- simplifyPattern recMode el (UniOp (OpTupleNth k tupleCount) letExpr) expr
    return (pSimpl, k+1)) (expr, 0) ([el] ++ restEls)
  unmarkTrace ast
  addExprAnnot $ return tSimpl

simplifyComplexExpression :: ComplexExpression -> Infer Expr
simplifyComplexExpression (ECTyped typeExpr) = do
  scheme <- resolveTypeExpression typeExpr
  addExprAnnot $ return $ Typed scheme
simplifyComplexExpression ast@(ECFor varName initExpr dir endExpr bodyExpr) = do
  markTrace ast
  forCheckType <- return $ Forall [] $ (TCon "Int")
  initSimpl <- simplifyComplexExpression initExpr
  endSimpl <- simplifyComplexExpression endExpr
  initSimplCheck <- return $ Check initSimpl forCheckType
  endSimplCheck <- return $ Check endSimpl forCheckType
  bodySimpl <- simplifyComplexExpression bodyExpr
  unmarkTrace ast
  addExprAnnot $ return $ Let varName (Op OpSame initSimplCheck endSimplCheck) bodySimpl
simplifyComplexExpression ast@(ECWhile condExpr bodyExpr) = do
  markTrace ast
  whileCheckType <- return $ Forall [] $ (TCon "Bool")
  condSimpl <- simplifyComplexExpression condExpr
  bodySimpl <- simplifyComplexExpression bodyExpr
  condSimplCheck <- return $ Check condSimpl whileCheckType
  id <- freshIdent
  unmarkTrace ast
  addExprAnnot $ return $ Let id condSimplCheck bodySimpl
  --bodyTypeExpr <- return $ TypeExprSimple $ TypeSExprEmpty
  --simplifyComplexExpression $ ECLet LetRecNo (PatIdent $ Ident "x") [] (TypeConstrDef bodyTypeExpr) (ECLet LetRecNo (PatIdent $ varName) [] TypeConstrEmpty initExpr bodyExpr) $ ECExpr $ ExprVar $ Ident "x"

simplifyComplexExpression ECExportEnv = addExprAnnot $ return Export
simplifyComplexExpression (ECTuple tuple) = addExprAnnot $ simplifyTuple tuple
simplifyComplexExpression (ECExpr expr) = addExprAnnot $ simplifyExpression expr
simplifyComplexExpression ast@(ECIf cond exprThen exprElse) = do
  markTrace ast
  condSimpl <- simplifyComplexExpression cond
  thenSimpl <- simplifyComplexExpression exprThen
  elseSimpl <- simplifyComplexExpression exprElse
  unmarkTrace ast
  addExprAnnot $ return $ If condSimpl thenSimpl elseSimpl
simplifyComplexExpression ast@(ECMatch expr _ clauses) = do
  markTrace ast
  clausesList <- foldrM (\(MatchClause pat clauseExpr) acc -> do
    r <- return $ ListElement $ ECLet LetRecNo pat [] TypeConstrEmpty expr clauseExpr
    return $ [r] ++ acc) [] clauses
  r <- simplifyComplexExpression $ ECExpr $ ExprList $ DList clausesList
  unmarkTrace ast
  addExprAnnot $ return $ UniOp OpHead r

simplifyComplexExpression ast@(ECFunction bPip matchClauses) = do
  id <- freshIdent
  markTrace ast
  r <- simplifyComplexExpression $ ECFun (PatIdent id) [] $ ECMatch (ECExpr $ ExprVar id) bPip matchClauses
  unmarkTrace ast
  addExprAnnot $ return r
simplifyComplexExpression ast@(ECFun pat argsPat expr) = do
  id <- freshIdent
  markTrace ast
  r <- simplifyComplexExpression (ECLet LetRecNo (PatIdent id) ([pat] ++ argsPat) TypeConstrEmpty expr (ECExpr $ ExprVar id))
  unmarkTrace ast
  addExprAnnot $ return r
simplifyComplexExpression (ECLetOperator recK opName patArgs letExpr expr) = do
  --Ident $ getOperatorName opName
  addExprAnnot $ simplifyComplexExpression (ECLet recK (PatIdent $ Ident $ getOperatorName opName) patArgs TypeConstrEmpty letExpr expr)
simplifyComplexExpression ast@(ECLet recK pat [] typeAnnot letExpr expr) = do
  markTrace ast
  expr <- withTypeAnnot typeAnnot expr
  letSimpl <- simplifyComplexExpression letExpr
  exprSimpl <- simplifyComplexExpression expr
  r <- simplifyPattern (isRec recK) pat letSimpl exprSimpl
  unmarkTrace ast
  addExprAnnot $ return r
simplifyComplexExpression (ECChecked expr typeExpr) = do
  simpl <- simplifyComplexExpression expr
  s <- resolveTypeExpression typeExpr
  addExprAnnot $ return $ Check simpl s
simplifyComplexExpression ast@(ECLet recK pat argsPats typeAnnot letExpr expr) = do
  markTrace ast
  expr <- withTypeAnnot typeAnnot expr
  letSimpl <- simplifyComplexExpression letExpr
  exprSimpl <- simplifyComplexExpression expr
  letSimplAcc <- foldrM (\pat expr -> do
    id <- freshIdent
    s <- simplifyPattern False pat (Var $ id) expr
    return $ Lam id s) letSimpl argsPats
  r <- simplifyPattern (isRec recK) pat letSimplAcc exprSimpl
  unmarkTrace ast
  addExprAnnot $ return r

simplifyTuple :: DTuple -> Infer Expr
simplifyTuple (DTuple firstElem elems) = do
  elemsSimpl <- foldM (\expr (DTupleElement el) -> do
    k <- simplifyExpression el
    return $ Op OpTupleCons k expr) (UniOp OpEmptyTuple Skip) ([firstElem] ++ elems)
  addExprAnnot $ return $ elemsSimpl

simplifyList :: DList -> Infer Expr
simplifyList (DList elems) = do
  elemsSimpl <- foldM (\expr (ListElement el) -> do
    k <- simplifyComplexExpression el
    return $ Op OpCons k expr) (UniOp OpEmptyList Skip) elems
  addExprAnnot $ return $ elemsSimpl

simplifyExpression :: Expression -> Infer Expr
simplifyExpression (ExprSemi expA expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op OpSemicolon simplA simplB
simplifyExpression (ExprOp opName) = do
  addExprAnnot $ return $ Var $ Ident $ getOperatorName opName
simplifyExpression (Expr1 expA (OperatorA name) expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression (Expr2 expA (OperatorB name) expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression (Expr3 expA (OperatorC name) expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression (Expr4 expA (OperatorD name) expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression (Expr4S expA OperatorDS expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op (OpCustom "*") simplA simplB
simplifyExpression (Expr5 expA (OperatorE name) expB) = do
  simplA <- simplifyExpression expA
  simplB <- simplifyExpression expB
  addExprAnnot $ return $ Op (OpCustom name) simplA simplB
simplifyExpression (Expr6 (OperatorF name) exp) = do
  simpl <- simplifyExpression exp
  addExprAnnot $ return $ UniOp (OpCustomUni name) simpl
simplifyExpression (ExprVar name) = addExprAnnot $ return $ Var name
simplifyExpression (ExprConst (CInt val)) = addExprAnnot $ return $ Lit $ LInt val
simplifyExpression (ExprConst (CString val)) = addExprAnnot $ return $ Lit $ LString val
simplifyExpression (ExprConst (CBool CBTrue)) = addExprAnnot $ return $ Lit $ LBool True
simplifyExpression (ExprConst (CBool CBFalse)) = addExprAnnot $ return $ Lit $ LBool False
simplifyExpression (ExprList list) = addExprAnnot $ simplifyList list
simplifyExpression (ExprCompl expr) = addExprAnnot $ simplifyComplexExpression expr
simplifyExpression ast@(ExprCall exp firstArg restArgs) = do
  markTrace ast
  fnExpr <- simplifyExpression exp
  r <- foldlM (\simpl argExp -> do
    argSimpl <- simplifySimpleExpression argExp
    return $ App simpl argSimpl) (fnExpr) ([firstArg] ++ restArgs)
  unmarkTrace ast
  addExprAnnot $ return r

simplifySimpleExpression :: SimpleExpression -> Infer Expr
simplifySimpleExpression (ESOp opName) = simplifyExpression $ ExprOp opName
simplifySimpleExpression (ESConst const) = simplifyExpression $ ExprConst const
simplifySimpleExpression (ESIdent name) = simplifyExpression $ ExprVar name
simplifySimpleExpression (ESExpr expr) = simplifyComplexExpression expr
simplifySimpleExpression (ESList list) = simplifyList list

inferComplexExpression :: ComplexExpression -> Infer (Type, [AConstraint])
inferComplexExpression ast = do
  tree <- simplifyComplexExpression ast
  infer tree

inferE :: Expr -> Infer (Env, Type, [AConstraint])
inferE expr = do
  env <- ask
  (t, c) <- infer expr
  return $ (env, t, c)

data AConstraint = AConstraint TypeErrorPayload Constraint

constraintDeannot :: AConstraint -> Constraint
constraintDeannot (AConstraint _ ac) = ac

constraintDeannotList :: [AConstraint] -> [Constraint]
constraintDeannotList acl = map constraintDeannot acl

constraintAnnot :: Constraint -> Infer AConstraint
constraintAnnot constrnt = do
  payl <- errPayload
  return $ AConstraint payl constrnt

constraintAnnotList :: [Constraint] -> Infer [AConstraint]
constraintAnnotList cs = do
  foldrM (\c acc -> do
    ca <- constraintAnnot c
    return $ [ca] ++ acc)  [] cs

infer :: Expr -> Infer (Type, [AConstraint])
infer expr = case expr of
  Skip -> return (typeInt, [])
  Lit (LInt _)  -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])
  Lit (LString _) -> return (typeString, [])

  Annot l t -> do
    s <- get
    put s{lastInferExpr = l}
    infer t

  Typed (Forall _ t) ->
    return (t, [])

  Export -> do
    env <- ask
    return (TExport env, [])

  Check e (Forall _ t) -> do
    (t1, c1) <- infer e
    ac <- constraintAnnotList [(t1, t)]
    return (t1, c1 ++ ac)

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
    ac <- constraintAnnotList [(t1, t2 `TArr` tv)]
    return (tv, c1 ++ c2 ++ ac)

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
    ac <- constraintAnnotList [(tv `TArr` tv, t1)]
    return (tv, c1 ++ ac)

  UniOp (OpCustomUni name) e1 -> do
    infer (App (Var $ Ident name) e1)

  UniOp op e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    u1 <- return $ t1 `TArr` tv
    u2 <- opsUni op
    ac <- constraintAnnotList [(u1, u2)]
    return (tv, c1 ++ ac)

  Op (OpCustom name) e1 e2 -> do
    infer (App (App (Var $ Ident name) e1) e2)

  Op op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    u1 <- return $ t1 `TArr` (t2 `TArr` tv)
    u2 <- ops op
    ac <- constraintAnnotList [(u1, u2)]
    return (tv, c1 ++ c2 ++ ac)

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    ac <- constraintAnnotList [(t1, typeBool), (t2, t3)]
    return (t2, c1 ++ c2 ++ c3 ++ ac)

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TList a)  = fv a
    fv (TTuple a b) = fv a ++ fv b
    fv TUnit = []
    fv (TExport _) = []
    fv (TCon _)   = []
    fv (TDep _ deps) = foldl (\acc el -> acc ++ (fv el)) [] deps

    normtype TUnit = TUnit
    normtype (TExport v) = (TExport v)
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TTuple a b) = TTuple (normtype a) (normtype b)
    normtype (TList a) = TList (normtype a)
    normtype (TCon a)   = TCon a
    normtype (TDep name deps) = TDep name $ map normtype deps
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

checkpointAnnotSolve :: AConstraint -> Solve ()
checkpointAnnotSolve (AConstraint l _) = do
    s <- get
    put s{lastAnnot = l}
    return ()

errSolvePayload :: Solve TypeErrorPayload
errSolvePayload = do
  s <- get
  lastAnnot <- return $ let SolveState { lastAnnot = lastAnnot } = s in lastAnnot
  return lastAnnot

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [AConstraint] -> Either TypeError Subst
runSolve cs = let st = (emptySubst, cs) in
  case runExcept $ runStateT (solver st) initSolve of
    Left e -> Left e
    Right (s, _) -> Right s

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = do
  payl <- errSolvePayload
  throwError $ UnificationMismatch payl t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies t1@(TDep name1 deps1) t2@(TDep name2 deps2) = do
  payl <- errSolvePayload
  _ <- if not (name1 == name2) then throwError $ UnificationMismatch payl [t1] [t2] else return 0
  unifyMany deps1 deps2
unifies (TList t1) (TList t2) = unifyMany [t1] [t2]
unifies (TTuple t1 t2) (TTuple t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = do
  payl <- errSolvePayload
  throwError $ UnificationFail payl t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((AConstraint l (t1, t2)): cs0) -> do
      checkpointAnnotSolve (AConstraint l (t1, t2))
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = do
                             payl <- errSolvePayload
                             throwError $ InfiniteType payl a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

