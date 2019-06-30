module Inference.Inferencer where

import Inference.Syntax
import Inference.TypingEnvironment
import Inference.Types
import Inference.Substitutions
import Inference.Errors
import Inference.Simplifier
import Inference.ConstraintSolver
import Inference.InferencerUtils
import Inference.TypeExpressionResolver

import Syntax.Base hiding (TypeConstraint)
import qualified Syntax.Base as Syntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable

import System.IO.Unsafe

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Run the inference monad
runInfer :: Env -> InferState -> Infer (Env, Type, [TypeConstraint]) -> IO (Either TypeError ((Env, Type, [TypeConstraint]), InferState))
runInfer env state fn = do
  --let v = runExcept $ runStateT (runReaderT m env) state in
  v <- runExceptT (runReaderT (runStateT fn (state)) (env))
  case v of
    (Left e) -> return $ Left e
    (Right ((env, t, c), state)) -> return $ Right ((env, t, c), state)

solve :: Either TypeError (Type, [TypeConstraint]) -> IO (Either TypeError Scheme)
solve r = case r of
  Left err -> return $ Left err
  Right (ty, cs) -> do
    s <- runSolve cs
    case s of
      Left err -> return $ Left err
      Right subst -> return $ Right $ closeOver $ subst .> ty

unpackEnvTypeContraints :: Either TypeError ((Env, Type, [TypeConstraint]), InferState) -> Either TypeError (Type, [TypeConstraint])
unpackEnvTypeContraints (Left r) = Left r
unpackEnvTypeContraints (Right ((_, t, c),_)) = Right (t, c)

retrieveEnv :: Either TypeError ((Env, Type, [TypeConstraint]), InferState) -> Env
retrieveEnv (Left r) = empty
retrieveEnv (Right ((e,_,_),_)) = e

retrieveState :: Either TypeError ((Env, Type, [TypeConstraint]), InferState) -> InferState
retrieveState (Left r) = initInfer
retrieveState (Right (_,state)) = state

-- | Solve for the toplevel type of an expression in a given environment
inferAST :: Env -> InferState -> Implementation -> IO (Either TypeError (Scheme, Env, InferState))
inferAST env state ex = do
  i <- runInfer env state (inferImplementation ex)
  env <- return $ retrieveEnv i
  state <- return $ retrieveState i
  scheme <- solve $ unpackEnvTypeContraints i
  case scheme of
    Left e -> return $ Left e
    Right s -> return $ Right (s, env, state)

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> InferState -> SimplifiedExpr -> IO (Either TypeError ([TypeConstraint], Subst, Type, Scheme))
constraintsExpr env state ex = do
  r <- runInfer env state (inferE ex)
  case r of
    Left err -> return $ Left err
    Right ((_, ty, cs), state) -> do
      s <- runSolve cs
      case s of
        Left err -> return $ Left err
        Right subst -> return $ Right (cs, subst, ty, sc)
          where
            sc = closeOver $ subst .> ty

-- | Canonicalize and return the polymorphic toplevel type.

ops :: Binop -> Infer Type
ops OpSemicolon = do
  tv1 <- fresh
  tv2 <- fresh
  return $ tv1 `TypeArrow` (tv2 `TypeArrow` tv2)
ops OpSame = do
  tv <- fresh
  return $ tv `TypeArrow` (tv `TypeArrow` tv)
ops OpCons = do
  tv <- fresh
  return $ (tv) `TypeArrow` ((TypeList tv) `TypeArrow` (TypeList tv) )
ops OpTupleCons = do
  tv <- fresh
  tv2 <- fresh
  tv3 <- fresh
  return $ (tv) `TypeArrow` ((TypeTuple tv2 tv3) `TypeArrow` (TypeTuple tv (TypeTuple tv2 tv3)))

opsUni :: Uniop -> Infer Type
opsUni OpHead = do
  tv <- fresh
  return $ (TypeList tv) `TypeArrow` (tv)
opsUni OpTails = do
  tv <- fresh
  return $ (TypeList tv) `TypeArrow` (TypeList tv)
opsUni OpEmptyList = do
  tv <- fresh
  tv2 <- fresh
  return $ tv `TypeArrow` (TypeList tv2)
opsUni OpEmptyTuple = do
  tv <- fresh
  return $ tv `TypeArrow` (TypeTuple TypeUnit TypeUnit)
opsUni OpListNth = do
  tv <- fresh
  return $ (TypeList tv) `TypeArrow` tv
opsUni (OpTupleNth index len) = do
  (tupleType, elsTypes) <- foldrM (\_ (tup, tvs) -> do
    tv <- fresh
    return $ ((TypeTuple tv tup), [tv] ++ tvs)) ((TypeTuple TypeUnit TypeUnit), []) (replicate len 0)
  return $ (tupleType) `TypeArrow` (elsTypes !! index)

inferImplementation :: Implementation -> Infer (Env, Type, [TypeConstraint])
inferImplementation ast@(IRoot cores) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) core -> do
    i <- local (\_ -> envAcc) $ inferImplementationCore core
    return i) (env, TypeUnit, []) cores
  unmarkTrace ast
  return r

inferImplementationCore :: ImplementationCore -> Infer (Env, Type, [TypeConstraint])
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
    return i) (env, TypeUnit, []) phrases
  unmarkTrace ast
  return r

createTypeExpressionAbstractArgConstructor :: Ident -> [String] -> TypeExpression
createTypeExpressionAbstractArgConstructor typeName [] =
  TypeExprSimple $ TypeSExprIdent $ typeName
createTypeExpressionAbstractArgConstructor typeName names@(hNames:tNames) =
  let (identHead:identTail) = map (\e -> TypeArgEl $ TypeExprSimple $ TypeSExprAbstract $ TypeIdentAbstract $ e) names in
   TypeExprIdent (TypeArgJust identHead identTail) typeName

inferVariantOption :: [String] -> Ident -> TDefVariant -> Infer (Env, Type, [TypeConstraint])
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

inferTypeDef :: TypeDef -> Infer (Env, Type, [TypeConstraint])
inferTypeDef ast@(TypeDefVar typeParams name options) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) option -> do
    i <- local (\_ -> envAcc) $ inferVariantOption (typeParamsToList typeParams) name option
    return i) (env, TypeUnit, []) options
  unmarkTrace ast
  return r
inferTypeDef ast@(TypeDefVarP typeParams name options) = do
  markTrace ast
  env <- ask
  r <- foldlM (\(envAcc, _, _) option -> do
    i <- local (\_ -> envAcc) $ inferVariantOption (typeParamsToList typeParams) name option
    return i) (env, TypeUnit, []) options
  unmarkTrace ast
  return r

inferImplementationPhrase :: ImplPhrase -> Infer (Env, Type, [TypeConstraint])
inferImplementationPhrase (IGlobalLetOperator recK opName restPatterns letExpr) = do
  (t, c) <- inferComplexExpression (ECLetOperator recK opName restPatterns letExpr $ ECExportEnv)
  return $ let (TypeAnnotated (AnnotationEnv exportedEnv)) = t in (exportedEnv, t, c)
inferImplementationPhrase (IGlobalLet recK
 pattern restPatterns typeAnnot letExpr) = do
  (t, c) <- inferComplexExpression (ECLet recK pattern restPatterns typeAnnot letExpr $ ECExportEnv)
  return $ let (TypeAnnotated (AnnotationEnv exportedEnv)) = t in (exportedEnv, t, c)
inferImplementationPhrase (IDefType typeDef) = inferTypeDef typeDef

inferComplexExpression :: ComplexExpression -> Infer (Type, [TypeConstraint])
inferComplexExpression ast = do
  tree <- simplifyComplexExpression resolveTypeExpression ast
  infer tree

inferE :: SimplifiedExpr -> Infer (Env, Type, [TypeConstraint])
inferE expr = do
  env <- ask
  (t, c) <- infer expr
  return $ (env, t, c)

infer :: SimplifiedExpr -> Infer (Type, [TypeConstraint])
infer SSkip = return ((TypeStatic "Int"), [])
infer (SConst (LInt _)) = return ((TypeStatic "Int"), [])
infer (SConst (LBool _)) = return ((TypeStatic "Bool"), [])
infer (SConst (LString _)) = return ((TypeStatic "String"), [])
infer (SAnnotated l t) = do
  s <- get
  put s{lastInferExpr = l}
  infer t
infer (STyped (Forall _ t)) =
  return (t, [])
infer (SExportEnv) = do
  env <- ask
  return (TypeAnnotated (AnnotationEnv env), [])
infer (SCheck e (Forall _ t)) = do
  (t1, c1) <- infer e
  ac <- constraintAnnoTypeList [TypeConstraint EmptyPayload (t1, t)]
  return (t1, c1 ++ ac)
infer (SVariable x) = do
    t <- lookupEnv x
    return (t, [])
infer (SFunction x e) = do
  tv <- fresh
  (t, c) <- (x, Forall [] tv) ==> (infer e)
  return (tv `TypeArrow` t, c)
infer (SCall e1 e2) = do
  (t1, c1) <- infer e1
  (t2, c2) <- infer e2
  tv <- fresh
  ac <- constraintAnnoTypeList [TypeConstraint EmptyPayload (t1, t2 `TypeArrow` tv)]
  return (tv, c1 ++ c2 ++ ac)
infer (SLet x e1 e2) = do
  env <- ask
  (t1, c1) <- infer e1
  s <- lift $ lift $ lift $ runSolve c1
  case s of
    Left err -> throwError err
    Right sub -> do
        let sc = generalize (sub .> env) (sub .> t1)
        (t2, c2) <- (x, sc) ==> (local (sub .>) (infer e2))
        return (t2, c1 ++ c2)
infer (SFixPoint e1) = do
  (t1, c1) <- infer e1
  tv <- fresh
  ac <- constraintAnnoTypeList [TypeConstraint EmptyPayload (tv `TypeArrow` tv, t1)]
  return (tv, c1 ++ ac)
infer (UniOp (OpCustomUni name) e1) = do
  infer (SCall (SVariable $ Ident name) e1)
infer (UniOp op e1) = do
  (t1, c1) <- infer e1
  tv <- fresh
  u1 <- return $ t1 `TypeArrow` tv
  u2 <- opsUni op
  ac <- constraintAnnoTypeList [TypeConstraint EmptyPayload (u1, u2)]
  return (tv, c1 ++ ac)
infer (Op (OpCustom name) e1 e2) = do
  infer (SCall (SCall (SVariable $ Ident name) e1) e2)
infer (Op op e1 e2) = do
  (t1, c1) <- infer e1
  (t2, c2) <- infer e2
  tv <- fresh
  u1 <- return $ t1 `TypeArrow` (t2 `TypeArrow` tv)
  u2 <- ops op
  ac <- constraintAnnoTypeList [TypeConstraint EmptyPayload (u1, u2)]
  return (tv, c1 ++ c2 ++ ac)
infer (SIf cond tr fl) = do
  (t1, c1) <- infer cond
  (t2, c2) <- infer tr
  (t3, c3) <- infer fl
  ac <- constraintAnnoTypeList [(TypeConstraint EmptyPayload (t1, TypeStatic "Bool")), (TypeConstraint EmptyPayload (t2, t3))]
  return (t2, c1 ++ c2 ++ c3 ++ ac)

