import Buildtools
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)

main = do
--     stackDir <- return ".stack-work"
--     executeTasks $ do
--         want ["_build/docs/index.html"]
--         ["_build/docs/*"] &%> \[ouths] -> do
--             executeCommandStack ["exec", "tintin", "run"] "."
--             executeCommand "cp" (["-R", (stackDir </> "tintin" </> "rendered" </> "."), ("_build/docs")]) "."
--             return ()
    defaultMain
