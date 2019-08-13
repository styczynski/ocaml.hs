module Buildutils (
    module Development.Shake
    , module Development.Shake.FilePath
    , decodeString
    , grepShCommand
    , getStackInstallDir
    , executeCommand
    , executeCommandStack
    , glob
    , finish
    , cp
    , message
    , executeTasks
) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Shelly (run, liftIO, Sh, shelly, silently, cd, rm_rf)
import qualified Shelly as Shelly
import Data.Text (pack, unpack)
import qualified System.FilePath.Glob as Glob
import Filesystem.Path.CurrentOS (decodeString)
import Text.Regex.TDFA

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

glob :: String -> Action [FilePath]
glob = liftIO . Glob.glob

message :: String -> Action ()
message = liftIO . putStrLn

finish :: Action ()
finish = return ()

cp :: FilePath -> FilePath -> Action ()
cp src dest = liftIO $ shelly $ Shelly.cp (decodeString src) (decodeString dest)

executeTasks :: Rules () -> IO ()
executeTasks taskDefs = shelly $ silently $ do
    liftIO $ shakeArgs shakeOptions{shakeFiles="_build"} taskDefs