{-# LANGUAGE ScopedTypeVariables #-}

import Lib
import Runtime
import Environment
import AbsSyntax
import Interpreter

import qualified Data.Text as T
import System.IO.Unsafe
import qualified Data.Map as Map

import GHCJS.Prim
import GHCJS.Foreign(toJSBool, jsUndefined, isString, isNumber, isBoolean)
import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object
import Data.Maybe (fromJust, isJust)

foreign import javascript unsafe "runParser = $1"
    set_callback :: Callback (JSVal -> IO JSVal) -> IO ()

runtimeValueFromJS :: JSVal -> IO RuntimeValue
runtimeValueFromJS val
  | isNull val = return $ RUnit
  | isUndefined val = return $ RUnit
  | isString val = do
    (input :: String) <- unpack . fromJust <$> fromJSVal val
    return $ RString $ input
  | isNumber val = do
      (input :: Int) <- fromJust <$> fromJSVal val
      return $ RInt $ toInteger $ input
  | isBoolean val = do
        (input :: Bool) <- fromJust <$> fromJSVal val
        return $ RBool $ input

runtimeValueToJS :: RuntimeValue -> Environment -> JSVal
runtimeValueToJS RUnit _ = jsNull
runtimeValueToJS (RInt val) _ = toJSInt $ fromIntegral val
runtimeValueToJS (RString val) _ = jsval $ pack val
runtimeValueToJS (RBool True) _ = toJSBool True
runtimeValueToJS (RBool False) _ = toJSBool False
runtimeValueToJS (RFunc _ body _) env = jsval $ unsafePerformIO $ syncCallback1' $ \jv -> do
  result <- execFunction body (map (unsafePerformIO . runtimeValueFromJS) $ unsafePerformIO $ fromJSArray $ jv) env
  callback <- syncCallback1' $ \jv -> do
      (input :: String) <- unpack . fromJust <$> fromJSVal jv
      ret <- executeCode input (getProgramEnvironmentDefault result env)
      return $ ret
  return $ runtimeValueToJS (getProgramResult result) (getProgramEnvironmentDefault result env)
runtimeValueToJS val _ = jsval $ pack $ show val

resultToJS :: ProgramResult -> Environment -> JSVal
resultToJS (FailedParse err) _ = jsval $ pack $ err
resultToJS (FailedExecution err) _ = jsval $ pack $ err
resultToJS (Executed val _) env = runtimeValueToJS val env

resultToErrorJS :: ProgramResult -> JSVal
resultToErrorJS (FailedParse _) = jsval $ pack "ParseError"
resultToErrorJS (FailedExecution _) = jsval $ pack "RuntimeError"
resultToErrorJS (Executed _ _) = jsNull

resultToEnvJS :: ProgramResult -> JSVal
resultToEnvJS (Executed _ env) = environmentToJS env
resultToEnvJS _ = jsNull

resultToFunSigJS :: ProgramResult -> IO JSVal
resultToFunSigJS (Executed (RFunc (RFuncSignature argsT retT) _ names) _) = do
  (o :: Object) <- create
  setProp (pack "args_count") (toJSInt $ length argsT) o
  setProp (pack "args_names") (unsafePerformIO $ toJSArray $ map (\(Ident name) -> jsval $ pack $ name) names) o
  return $ jsval o
resultToFunSigJS _ = return $ jsNull

environmentToJS :: Environment -> JSVal
environmentToJS e = unsafePerformIO $ toJSArray $ map (\((Ident key), value) -> unsafePerformIO $ toJSArray [jsval $ pack key, runtimeValueToJS value e]) $ Map.toList $ variables e

executeCode input env = do
  (o :: Object) <- create
  result <- return $ unsafePerformIO $ runWith 2 input env
  setProp (pack "resultStr" :: JSString) (jsval . pack $ (resultToStr result)) o
  setProp (pack "result") (resultToJS result (getProgramEnvironmentDefault result env)) o
  setProp (pack "error") (resultToErrorJS result) o
  setProp (pack "env") (resultToEnvJS result) o
  setProp (pack "funSig") (unsafePerformIO $ resultToFunSigJS result) o
  callback <- syncCallback1' $ \jv -> do
    (input :: String) <- unpack . fromJust <$> fromJSVal jv
    ret <- executeCode input (getProgramEnvironmentDefault result env)
    return $ ret
  set_callback callback
  return $ jsval o

main :: IO ()
main = do
    callback <- syncCallback1' $ \jv -> do
      (input :: String) <- unpack . fromJust <$> fromJSVal jv
      ret <- executeCode input emptyEnv
      return $ ret
    set_callback callback
