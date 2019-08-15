import Buildtools
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)

main = do
    stackDir <- return ".stack-work"
    executeTasks $ do
        want ["_build/index.html"]
        ["_build/*"] &%> \[ouths] -> do
            executeCommandStack ["upgrade", "--binary-version", "1.6.3"] "."
            executeCommand "yarn" ["install"]
            executeCommand "yarn" ["build"]
            executeCommand "cp" (["-R", ("./build/."), ("_build")]) "."
            return ()
