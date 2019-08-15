import Buildtools
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)

customHook originalFn = do
    executeTasks $ do
        want ["test/Generated/BaseSpec.hs"]
        ["test/Generated/*.hs"] &%> \[ouths] -> do
            executeCommandStack ["exec", "test-preprocessor", "--", "-i", "../../examples", "-o", "./test/Generated", "-p", "Generated."] "."
            finish
    originalFn

main = defaultMainWithHooks $ let UserHooks { readDesc = readDescDefault } = simpleUserHooks in simpleUserHooks { readDesc = (customHook readDescDefault) }
