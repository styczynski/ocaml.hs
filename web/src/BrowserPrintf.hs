{-|
Module      : Interop.Printf
Description : Variadic printf function
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This file contains definition of printf function that can be used to print values from the insides
  of the interpreter.
-}
{-# LANGUAGE ExistentialQuantification #-}
module BrowserPrintf where

import GHCJS.Prim
import GHCJS.Foreign(toJSBool, jsUndefined, isString, isNumber, isBoolean)
import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object
import Data.Maybe (fromJust, isJust)
import System.IO.Unsafe

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader

import           Runtime.Runtime
import           Runtime.Environment

import           Text.Printf

import           System.IO

data PrintfArgT = forall a. PrintfArg a => P a

foreign import javascript unsafe "window.logConsole($1)"
    call_native_printf :: JSVal -> IO JSVal

printIt str =
  let _ = unsafePerformIO $ call_native_printf $ jsval $ pack str in
  printf str

browserPrintfa :: PrintfType t => String -> [PrintfArgT] -> t
browserPrintfa format = browserPrintfa' format . reverse
 where
  browserPrintfa' :: PrintfType t => String -> [PrintfArgT] -> t
  browserPrintfa' format []         = printIt ((printf format) :: String)
  browserPrintfa' format (P a : as) = browserPrintfa' format as a

browserPrintfUnpack :: RuntimeValue -> PrintfArgT
browserPrintfUnpack (RInt    v) = P v
browserPrintfUnpack (RBool   v) = P (show v)
browserPrintfUnpack (RString v) = P v
browserPrintfUnpack v           = P (valueToStrN v)

browserPrintfVars :: PrintfType t => String -> [RuntimeValue] -> t
browserPrintfVars format vars = browserPrintfa format $ map browserPrintfUnpack vars

browser_printf_str :: [RuntimeValue] -> IO ()
browser_printf_str ((RString format) : args) =
  --putStrLn $ "format: [" ++ (format) ++ "] args: " ++ (show args)
  browserPrintfVars format args