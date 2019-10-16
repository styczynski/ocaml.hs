{-# LANGUAGE ScopedTypeVariables #-}
module Buildtools (
    module Development.Shake
    , module Distribution.Simple
    , module Distribution.Simple.Setup
    , module Distribution.Types.HookedBuildInfo
    , module Development.Shake.FilePath
    , decodeString
    , grepShCommand
    , getStackInstallDir
    , executeCommand
    , executeCommandStack
    , executeCommandX
    , executeCommandXEnv
    , executeCommandStackX
    , glob
    , finish
    , cp
    , message
    , executeTasks
    , executeStackBuild
    , executeSubTask
) where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo

import Development.Shake hiding (getEnv)
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Shelly (run, liftIO, Sh, shelly, silently, cd, rm_rf, catchany, setenv)
import qualified Shelly as Shelly
import Data.Text (pack, unpack)
import qualified System.FilePath.Glob as Glob
import Filesystem.Path.CurrentOS (decodeString)
import Text.Regex.TDFA

import System.Environment
import System.IO.Error

import Control.Exception.Base

executeStackBuild :: [String] -> String -> Action Bool
executeStackBuild cmd path = do
    t <- executeCommandStackX cmd path
    r <- (case t of
        Just s -> return $ if s == "ERROR" then False else True
        Nothing -> executeStackBuild cmd path)
    return r

handleStackBuildOutput :: String -> IO (Maybe String)
handleStackBuildOutput output = do
    matches <- return $ filter (\(_, _, _, m) -> length m > 0) $ map (\line -> (line =~ (".*(OCAMLHS_BUILD_DIRTY).*" :: String)) :: (String, String, String, [String])) $ lines output
    result <- return $ case matches of
        ((_, _, _, h:t):_) -> Nothing
        _ -> Just "ERROR"
    return result

grepShCommand :: String -> [String] -> String -> String -> Sh (Maybe [String])
grepShCommand command args cwd regex = do
    cd $ decodeString cwd
    host <- run (decodeString command) $ map pack args
    matches <- return $ filter (\(_, _, _, m) -> length m > 0) $ map (\line -> (line =~ regex) :: (String, String, String, [String])) $ lines $ unpack host
    result <- return $ case matches of
        ((_, _, _, h:t):_) -> Just $ h:t
        _ -> Nothing
    return result

getStackInstallDir :: String -> IO String
getStackInstallDir path = shelly $ silently $ do
    stackPathResult <- grepShCommand "stack" ["path", "--allow-different-user"] path "local-install-root: (.*)"
    stackPath <- return $ case stackPathResult of
        Nothing -> ""
        Just (h:_) -> h
    return stackPath

executeCommand :: String -> [String] -> String -> Action String
executeCommand command args cwd = liftIO $ shelly $ do
    cd $ decodeString cwd
    result <- run (decodeString command) $ map pack args
    return $ unpack result

executeCommandStack :: [String] -> String -> Action String
executeCommandStack args = executeCommand "stack" (["--allow-different-user"] ++ args)

executeCommandX :: String -> [String] -> String -> Action (Maybe String)
executeCommandX command args cwd = liftIO $ catchany (shelly $ do
    cd $ decodeString cwd
    result <- run (decodeString command) $ map pack args
    return $ Just $ unpack result) (\(e :: SomeException) -> do
        handleStackBuildOutput $ show e)

executeCommandXEnv :: String -> [String] -> String -> [(String, String)] -> Action (Maybe String)
executeCommandXEnv command args cwd env = liftIO $ catchany (shelly $ do
    cd $ decodeString cwd
    mapM (\(name, value) -> setenv (pack name) (pack value)) env
    result <- run (decodeString command) $ map pack args
    return $ Just $ unpack result) (\(e :: SomeException) -> do
        handleStackBuildOutput $ show e)

executeSubTask :: String -> String -> Action (Maybe String)
executeSubTask stackVersion path = do
  executeCommandXEnv "bash" ["-c", "unset GHC_PACKAGE_PATH && unset HASKELL_PACKAGE_SANDBOX && unset GHC_ENVIRONMENT && unset HASKELL_DIST_DIR && unset HASKELL_PACKAGE_SANDBOXES && printenv && stack upgrade --binary-version " ++ stackVersion ++ " && stack install shake --allow-different-user && stack exec -- shake"] path []

executeCommandStackX :: [String] -> String -> Action (Maybe String)
executeCommandStackX args = executeCommandX "stack" (["--allow-different-user"] ++ args)

glob :: String -> Action [FilePath]
glob = liftIO . Glob.glob

message :: String -> Action ()
message = liftIO . putStrLn

finish :: Action ()
finish = do
    liftIO $ setEnv "OCAMLHS_BUILD_DIRTY" "TRUE"
    message "Done"

cp :: FilePath -> FilePath -> Action ()
cp src dest = liftIO $ shelly $ Shelly.cp (decodeString src) (decodeString dest)

executeTasks :: Rules () -> IO ()
executeTasks taskDefs = do
    setEnv "OCAMLHS_BUILD_DIRTY" "FALSE"
    shelly $ silently $ do
        liftIO $ shake shakeOptions{shakeFiles="_build"} taskDefs
    buildDirtyEnv <- getEnv "OCAMLHS_BUILD_DIRTY"
    if buildDirtyEnv == "TRUE" then ioError $ userError "OCAMLHS_BUILD_DIRTY" else return ()
