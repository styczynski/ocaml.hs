{-# LANGUAGE ScopedTypeVariables #-}
import Buildtools

import Control.Exception.Base

main :: IO ()
main = do
    stackDir <- getStackInstallDir "."
    executeTasks $ do
        want [("_build/cli" <.> exe), "test"]

        "test" %> \out -> do
            alwaysRerun
            success <- executeStackBuild "test"
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "touch" [out] "."
            return ()

        ["_build/cli"] &%> \[ouths] -> do
            success <- executeStackBuild "build"
            _ <- liftIO $ if success then return () else ioError $ userError "Fail."
            executeCommand "cp" ([(stackDir </> "bin" </> "interpreter"), ("_build/cli" <.> exe)]) "."
            message "Done!"