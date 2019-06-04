module Runtime where

import AbsSyntax

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map


data Environment = Environment { variables :: (Map.Map Ident RuntimeValue) } deriving (Show)
data ExecutionResult = FailedParse String | FailedExecution String | Executed RuntimeValue Environment
type Exec = StateT (Environment) (ReaderT (Environment) (ExceptT String IO))

data RuntimeValue
  = REmpty
  | RInvalid
  | RInt Integer
  | RString String
  | RBool Bool
  deriving (Show, Eq)

valueToStr :: RuntimeValue -> String
valueToStr REmpty = "()"
valueToStr (RInt val) = show val
valueToStr (RString val) = show val
valueToStr (RBool val) = show val
valueToStr (RInvalid) = show "Invalid"

unpackBool :: RuntimeValue -> Exec Bool
unpackBool val =
  case val of
    RBool v -> return v
    _ -> throwError "Expected type: Bool"

unpackInt :: RuntimeValue -> Exec Integer
unpackInt val =
  case val of
    RInt v -> return v
    _ -> throwError "Expected type: Int"

unpackString :: RuntimeValue -> Exec String
unpackString val =
  case val of
    RString v -> return v
    _ -> throwError "Expected type: String"

vmapBool :: (Bool -> RuntimeValue) -> Exec RuntimeValue -> Exec RuntimeValue
vmapBool fn e1 = do
  val1 <- e1 >>= unpackBool
  return $ fn val1

vmapBool2 :: (Bool -> Bool -> RuntimeValue) -> Exec RuntimeValue -> Exec RuntimeValue -> Exec RuntimeValue
vmapBool2 fn e1 e2 = do
  val1 <- e1 >>= unpackBool
  val2 <- e2 >>= unpackBool
  return $ fn val1 val2

vmapInt :: (Integer -> RuntimeValue) -> Exec RuntimeValue -> Exec RuntimeValue
vmapInt fn e1 = do
  val1 <- e1 >>= unpackInt
  return $ fn val1

vmapInt2 :: (Integer -> Integer -> RuntimeValue) -> Exec RuntimeValue -> Exec RuntimeValue -> Exec RuntimeValue
vmapInt2 fn e1 e2 = do
  val1 <- e1 >>= unpackInt
  val2 <- e2 >>= unpackInt
  return $ fn val1 val2
