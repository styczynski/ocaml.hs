import Buildtools
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)

customHook originalFn = do
    executeTasks $ do
        want ["parser/TestSyntax.hs"]
        ["parser/*.hs"] &%> \[ouths] -> do
            executeCommandStack ["exec", "bnfc", "--", "-m", "-o", "_build/parser", ("grammar" </> "syntax.cf")] "."
            executeCommand "sed" ["-i", "-e", "s/module Main where/module TestSyntax where/g", "_build/parser/TestSyntax.hs"] "."
            executeCommandStack ["exec", "happy", "--", "-gca", "ParSyntax.y"] "./_build/parser"
            executeCommandStack ["exec", "alex", "--", "-g", "LexSyntax.x"] "./_build/parser"
            filesXToRemove <- glob "_build/parser/*.x"
            filesYToRemove <- glob "_build/parser/*.y"
            executeCommand "rm" (["-r", "-f", "-d"] ++ filesXToRemove ++ filesYToRemove) "."
            executeCommand "cp" (["-r", "_build/parser", "."]) "."
            executeCommand "sleep" ["5"] "."
            executeCommand "touch" ["parser"] "."
            finish
    originalFn

main = defaultMainWithHooks $ let UserHooks { readDesc = readDescDefault } = simpleUserHooks in simpleUserHooks { readDesc = (customHook readDescDefault) }
