{-# LANGUAGE ScopedTypeVariables #-}

import Init

import Lib
import Runtime.Runtime
import Runtime.Environment
import Syntax.Base
import Interpreter.Interpreter
import qualified Inference.Types as Types

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
  | isNull val = return $ REmpty
  | isUndefined val = return $ REmpty
  | isString val = do
    (input :: String) <- unpack . fromJust <$> fromJSVal val
    return $ RString $ input
  | isNumber val = do
      (input :: Int) <- fromJust <$> fromJSVal val
      return $ RInt $ toInteger $ input
  | isBoolean val = do
        (input :: Bool) <- fromJust <$> fromJSVal val
        return $ RBool $ input

createRuntimeValueWrapper :: String -> JSVal -> IO JSVal
createRuntimeValueWrapper typeName val = do
  (o :: Object) <- create
  setProp (pack "type" :: JSString) (jsval . pack $ typeName) o
  setProp (pack "value" :: JSString) val o
  return $ jsval o

runtimeValueToJS :: String -> RuntimeValue -> Environment -> IO JSVal
runtimeValueToJS t v env = createRuntimeValueWrapper t $ jsval $ pack (valueToStr env v)

resultToJS :: ExecutionResult -> Environment -> IO JSVal
resultToJS (FailedTypechecking err) _ = return $ jsval $ pack $ show err
resultToJS (FailedParse err) _ = return $ jsval $ pack $ err
resultToJS (FailedExecution err) _ = return $ jsval $ pack $ err
resultToJS (Executed val t _) env = runtimeValueToJS (Types.typeToStr [] t) val env

resultToErrorJS :: ExecutionResult -> JSVal
resultToErrorJS (FailedTypechecking _) = jsval $ pack "TypeError"
resultToErrorJS (FailedParse _) = jsval $ pack "ParseError"
resultToErrorJS (FailedExecution _) = jsval $ pack "RuntimeError"
resultToErrorJS (Executed _ _ _) = jsNull

resultToEnvJS :: ExecutionResult -> JSVal
resultToEnvJS (Executed _ _ env) = environmentToJS env
resultToEnvJS _ = jsNull

resultToFunSigJS :: ExecutionResult -> IO JSVal
resultToFunSigJS _ = return $ jsNull

environmentToJS :: Environment -> JSVal
environmentToJS e = unsafePerformIO $ toJSArray $ map (\((Ident key), value) -> unsafePerformIO $ toJSArray [jsval $ pack key, unsafePerformIO $ runtimeValueToJS "Environment" value e]) $ Map.toList $ variables e

executeCode input env = do
  (o :: Object) <- create
  result <- return $ unsafePerformIO $ runWith 2 input env
  setProp (pack "resultStr" :: JSString) (jsval . pack $ (resultToStr result)) o
  setProp (pack "result") (unsafePerformIO $ resultToJS result (getProgramEnvironmentDefault result env)) o
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
    initEnv0 <- runInitEmpty
    (Executed _ _ initEnv) <- runWith 0 initCode initEnv0
    callback <- syncCallback1' $ \jv -> do
      (input :: String) <- unpack . fromJust <$> fromJSVal jv
      ret <- executeCode input initEnv
      return $ ret
    set_callback callback
