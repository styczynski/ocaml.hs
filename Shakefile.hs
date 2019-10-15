{-# LANGUAGE ScopedTypeVariables #-}
import Buildtools

import Control.Exception.Base

main :: IO ()
main = do
    stackDir <- getStackInstallDir "."
    executeTasks $ do
        want ["_build/web"]

        "test.lock" %> \out -> do
            need [("_build/cli" <.> exe)]
            alwaysRerun
            executeCommandStack ["upgrade", "--binary-version", "2.1.3"] "."
            success <- executeStackBuild ["test", "--no-terminal", "--coverage", "--reconfigure"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "touch" [out] "."
            return ()

        ["_build/cli"] &%> \[ouths] -> do
            executeCommandStack ["upgrade", "--binary-version", "2.1.3"] "."
            success <- executeStackBuild ["build"] "."
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            stackNewDir <- liftIO $ getStackInstallDir "."
            executeCommand "cp" ([(stackNewDir </> "bin" </> "interpreter"), ("_build/cli" <.> exe)]) "."
            message "Done!"

        ["_build/web"] &%> \[ouths] -> do
            --need ["test.lock"]
            --("GHC_PACKAGE_PATH", ""), ("HASKELL_PACKAGE_SANDBOX", ""), ("GHC_ENVIRONMENT", ""), ("HASKELL_DIST_DIR", ""), ("ASKELL_PACKAGE_SANDBOXES", "")
            -- unset GHC_PACKAGE_PATH && unset HASKELL_PACKAGE_SANDBOX && unset GHC_ENVIRONMENT && unset HASKELL_DIST_DIR && unset HASKELL_PACKAGE_SANDBOXES
            executeSubTask "1.6.3" "./packages/web"
            --executeCommandStack ["build"] "./packages/web"
            --executeCommandStack ["exec", "--cwd", "./packages/web-frontend", "--", "shake"] "."
            message "Done web!"
