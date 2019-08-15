{-# LANGUAGE ScopedTypeVariables #-}
import Buildtools

import Control.Exception.Base

main :: IO ()
main = do
    stackDir <- getStackInstallDir "."
    executeTasks $ do
        want [("_build/cli" <.> exe), "test.lock"]

        "test.lock" %> \out -> do
            alwaysRerun
            success <- executeStackBuild ["test", "--no-terminal", "--coverage"]
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "touch" [out] "."
            return ()

        ["_build/cli"] &%> \[ouths] -> do
            success <- executeStackBuild ["build"]
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "cp" ([(stackDir </> "bin" </> "interpreter"), ("_build/cli" <.> exe)]) "."
            message "Done!"