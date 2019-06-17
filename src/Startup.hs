module Startup where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Runtime
import Environment
import ExportUtils
import Arithmetics
import Printf

failwith :: String -> IO ()
failwith str = error $ "Failed with: " ++ str

invalid_arg :: String -> IO ()
invalid_arg str = error $ "Invalid_argument: " ++ str

printf_str :: [RuntimeValue] -> IO ()
printf_str ((RString format):args) =
  --putStrLn $ "format: [" ++ (format) ++ "] args: " ++ (show args)
  printfVars format args

print_str :: RuntimeValue -> IO ()
print_str v = putStr $ valueStringifyRaw v

print_env :: RuntimeValue -> Exec String
print_env _ = do
  env <- ask
  runtimePrint $ "[Environment]: " ++ (envToStr env)
  return $ "Done."

ignore :: RuntimeValue -> RuntimeValue
ignore _ = REmpty

interpreterStartupFn :: Exec (RuntimeValue, Environment)
interpreterStartupFn = do
  e <- ask
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_add" "'a -> 'a -> 'a" valueAdd
  (_,e) <- local (\_ -> e) $ setNativeVariable "string_of_int" "Int -> String" valueStringify
  (_,e) <- local (\_ -> e) $ setNativeVariable "ignore" "'a -> unit" ignore
  (_,e) <- local (\_ -> e) $ setNativeVariable "get_line" "String -> String" runtimeGetLine
  (_,e) <- local (\_ -> e) $ setNativeVariable "print" "'a -> unit" print_str
  (_,e) <- local (\_ -> e) $ setNativeVariable "printf" "String -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> unit" (VarargFun printf_str)
  (_,e) <- local (\_ -> e) $ setNativeVariable "print_env" "'a -> unit" print_env
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_create_ref" "'a -> 'a Ref" valueCreateRef
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_set_ref" "'a Ref -> 'a -> unit" valueSetRef
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_get_ref" "'a Ref -> 'a" valueGetRef
  (_,e) <- local (\_ -> e) $ setNativeVariable "failwith" "String -> 'a" failwith
  (_,e) <- local (\_ -> e) $ setNativeVariable "invalid_arg" "String -> 'a" invalid_arg
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_eq" "'a -> 'a -> Bool" valueEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_lt" "'a -> 'a -> Bool" valueLt
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_gt" "'a -> 'a -> Bool" valueGt
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_lt_eq" "'a -> 'a -> Bool" valueLtEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_gt_eq" "'a -> 'a -> Bool" valueGtEq
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_cons" "'a -> ['a] -> ['a]" valueCons
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_join" "['a] -> ['a] -> ['a]" valueJoin
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_and" "Bool -> Bool -> Bool" valueAnd
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_or" "Bool -> Bool -> Bool" valueOr
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_not" "Bool -> Bool" valueNot
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_mod" "'a -> 'a -> 'a" valueMod
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_add" "'a -> 'a -> 'a" valueAdd
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_sub" "'a -> 'a -> 'a" valueSub
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_mul" "'a -> 'a -> 'a" valueMul
  (_,e) <- local (\_ -> e) $ setNativeVariable "value_div" "'a -> 'a -> 'a" valueDiv
  (_,e) <- local (\_ -> e) $ setNativeVariable "string_split" "String -> String -> [String]" valueSplit
  (_,e) <- local (\_ -> e) $ setNativeVariable "string_join" "String -> String -> String" valueJoin
  return (REmpty, e)