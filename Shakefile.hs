{-# LANGUAGE ScopedTypeVariables #-}
import Buildtools

import Control.Exception.Base

main :: IO ()
main = do
    stackDir <- getStackInstallDir "."
    executeTasks $ do
        want [("_build/cli" <.> exe), "test.lock", "_build/web"]

        "test.lock" %> \out -> do
            alwaysRerun
            success <- executeStackBuild ["test", "--no-terminal", "--coverage"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "touch" [out] "."
            return ()

        ["_build/cli"] &%> \[ouths] -> do
            success <- executeStackBuild ["build"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "cp" ([(stackDir </> "bin" </> "interpreter"), ("_build/cli" <.> exe)]) "."
            message "Done!"

        ["_build/web"] &%> \[ouths] -> do
            executeCommandStack ["upgrade", "--binary-version", "2.1.1"] "."
            executeCommandStack ["exec", "--cwd", "./packages/web-frontend", "--", "shake"] "."
            executeCommandStack ["exec", "--cwd", "./packages/web", "--", "shake"] "."
            message "Done web!"