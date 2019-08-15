import Buildtools
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)

main = do
    stackDir <- return ".stack-work"
    executeTasks $ do
        want ["_build/docs/index.html"]
        ["_build/docs/*"] &%> \[ouths] -> do
            executeCommandStack ["upgrade", "--binary-version", "1.6.3"] "."
            executeCommand "cp" (["-R", ("../web-frontend/_build/index.html"), ("_build/parser_index.html")]) "."
            executeCommand "cp" (["-R", ("../web-frontend/_build/."), ("_build")]) "."
            executeCommand "cp" (["-R", ("../doc/_build/docs"), ("_build")]) "."
            return ()
