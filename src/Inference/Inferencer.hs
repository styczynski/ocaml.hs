module Inference.Inferencer where

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Type
import Inference.Substitutions
import Inference.Errors
import Inference.Simplifier
import Inference.ConstraintSolver
import Inference.InferencerUtils

import AbsSyntax
import PrintSyntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import System.IO.Unsafe

import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------



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

resolveTypeExpression :: TypeExpression -> Infer Scheme
resolveTypeExpression exp = do
  fvs <- getTypeExpressionFV exp
  t <- resolveTypeExpressionRec fvs exp
  fvsT <- return $ Map.elems fvs
  return $ Forall fvsT t

inferComplexExpression :: ComplexExpression -> Infer (Type, [AConstraint])
inferComplexExpression ast = do
  tree <- simplifyComplexExpression resolveTypeExpression ast
  infer tree

inferE :: Expr -> Infer (Env, Type, [AConstraint])
inferE expr = do
  env <- ask
  (t, c) <- infer expr
  return $ (env, t, c)

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
    (t, c) <- (x, Forall [] tv) ==> (infer e)
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
            (t2, c2) <- (x, sc) ==> (local (apply sub) (infer e2))
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

