{-# LANGUAGE ExistentialQuantification #-}
module Printf where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime.Runtime
import Runtime.Environment

import Text.Printf

import System.IO

data PrintfArgT = forall a. PrintfArg a => P a

printfa :: PrintfType t => String -> [ PrintfArgT ] -> t
printfa format = printfa' format . reverse
  where printfa' :: PrintfType t => String -> [ PrintfArgT ] -> t
        printfa' format [] = printf format
        printfa' format (P a:as) = printfa' format as a

printfUnpack :: RuntimeValue -> PrintfArgT
printfUnpack (RInt v) = P v
printfUnpack (RBool v) = P (show v)
printfUnpack (RString v) = P v
printfUnpack v = P (valueToStrN v)

printfVars :: PrintfType t => String -> [ RuntimeValue ] -> t
printfVars format vars =
  printfa format $ map printfUnpack vars

runtimeGetLine :: RuntimeValue -> Exec RuntimeValue
runtimeGetLine (RString greeting) = do
  lift $ lift $ lift $ printfVars greeting []
  lift $ lift $ lift $ hFlush stdout
  val <- lift $ lift $ lift $ getLine
  lift $ lift $ lift $ hFlush stdout
  return $ RString val