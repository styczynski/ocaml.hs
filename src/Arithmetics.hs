module Arithmetics where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime

valueEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueEq (RInt a) (RInt b) = return $ a == b
valueEq (RString a) (RString b) = return $ a == b
valueEq (RBool a) (RBool b) = return $ a == b
valueEq REmpty REmpty = return True
valueEq _ _ = throwError "Failed comparison"

valueLt :: RuntimeValue -> RuntimeValue -> Exec Bool
valueLt (RInt a) (RInt b) = return $ a < b
valueLt _ _ = throwError "Failed comparison"

valueGt :: RuntimeValue -> RuntimeValue -> Exec Bool
valueGt (RInt a) (RInt b) = return $ a > b
valueGt _ _ = throwError "Failed comparison"

valueLtEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueLtEq (RInt a) (RInt b) = return $ a <= b
valueLtEq _ _ = throwError "Failed comparison"

valueGtEq :: RuntimeValue -> RuntimeValue -> Exec Bool
valueGtEq (RInt a) (RInt b) = return $ a >= b
valueGtEq _ _ = throwError "Failed comparison"