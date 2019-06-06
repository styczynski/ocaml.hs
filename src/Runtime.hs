module Runtime where

import AbsSyntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map


data Environment = Environment {
  variables     :: (Map.Map Ident RuntimeValue),
  refs          :: (Map.Map Integer RuntimeRefValue),
  nextFreeRef   :: Integer
}

data ExecutionResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment
type Exec = StateT (Environment) (ReaderT (Environment) (ExceptT String IO))

type RFunBody = [RuntimeValue] -> Exec (RuntimeValue, Environment)
data RFunSig = RFunSig Int

data RuntimeRefValue
  = RfFun RFunSig RFunBody
  | RfInvalid RuntimeValue

data RuntimeValue
  = REmpty
  | RInvalid
  | RInt Integer
  | RString String
  | RBool Bool
  | RRef Integer
  deriving (Show, Eq)

instance Show RuntimeRefValue where
  show (RfInvalid val) = "<Invalid " ++ (show val) ++ ">"
  show (RfFun _ _) = "<Function>"

resultToStr :: ExecutionResult -> String
resultToStr (Executed val _) = valueToStr val
resultToStr (FailedParse err) = err
resultToStr (FailedExecution err) = err

valueToStr :: RuntimeValue -> String
valueToStr REmpty = "()"
valueToStr (RInt val) = show val
valueToStr (RString val) = show val
valueToStr (RBool val) = show val
valueToStr (RRef _) = "<ref>"
valueToStr (RInvalid) = "Invalid"

unpackBool :: (RuntimeValue, Environment) -> Exec (Bool, Environment)
unpackBool (val, env) =
  case val of
    RBool v -> return (v, env)
    _ -> throwError "Expected type: Bool"

unpackInt :: (RuntimeValue, Environment) -> Exec (Integer, Environment)
unpackInt (val, env) =
  case val of
    RInt v -> return (v, env)
    v -> throwError $ "Expected type: Int got " ++ (show v)

unpackString :: (RuntimeValue, Environment) -> Exec (String, Environment)
unpackString (val, env) =
  case val of
    RString v -> return (v, env)
    _ -> throwError "Expected type: String"

vmapBool :: (Bool -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapBool fn e1 = do
  (val1, env) <- e1 >>= unpackBool
  return ((fn val1), env)

vmapBool2 :: (Bool -> Bool -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapBool2 fn e1 e2 = do
  (val1, env1) <- e1 >>= unpackBool
  (val2, env2) <- e2 >>= unpackBool
  return ((fn val1 val2), env2)

vmapInt :: (Integer -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapInt fn e1 = do
  (val1, env1) <- e1 >>= unpackInt
  return ((fn val1), env1)

vmapInt2 :: (Integer -> Integer -> RuntimeValue) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment) -> Exec (RuntimeValue, Environment)
vmapInt2 fn e1 e2 = do
  (val1, env1) <- e1 >>= unpackInt
  (val2, env2) <- e2 >>= unpackInt
  return ((fn val1 val2), env2)
