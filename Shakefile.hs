import Buildutils

main :: IO ()
main = executeTasks $ do

    want ["_build/test-preprocessor" <.> exe]

    ["parser/*.hs"] &%> \[ouths] -> do
        executeCommandStack ["exec", "bnfc", "--", "-m", "-o", "parser", "./grammar/syntax.cf"] "."
        executeCommand "sed" ["-i", "-e", "s/module Main where/module TestSyntax where/g", "parser/TestSyntax.hs"] "."
        executeCommandStack ["exec", "happy", "--", "-gca", "ParSyntax.y"] "./parser"
        executeCommandStack ["exec", "alex", "--", "-g", "LexSyntax.x"] "./parser"
        filesXToRemove <- glob "parser/*.x"
        filesYToRemove <- glob "parser/*.y"
        filesCabalToRemove <- glob "ocamlhs.cabal"
        executeCommand "rm" (["-r", "-f", "-d"] ++ filesXToRemove ++ filesYToRemove ++ filesCabalToRemove) "."
        message "Done!"

    "_build/interpreter" <.> exe %> \out -> do
        stackPath <- liftIO $ getStackInstallDir "."
        executeCommandStack ["upgrade", "--binary-version", "2.1.1"] "."
        executeCommandStack ["install", "--only-dependencies"] "."
        executeCommandStack ["install", "happy"] "."
        need ["parser/TestSyntax.hs"]
        srcFiles <- glob "src/**/*.hs"
        appFiles <- glob "app/**/*.hs"
        grammarFiles <- glob "grammar/**/*.cf"
        need $ srcFiles ++ appFiles ++ grammarFiles
        executeCommandStack ["build"] "."
        cp (stackPath </> "bin" </> "interpreter") out
        finish

    "_build/test-preprocessor" <.> exe %> \out -> do
        preprocessorStackPath <- liftIO $ getStackInstallDir "test-preprocessor"
        executeCommandStack ["build"] "test-preprocessor"
        cp (preprocessorStackPath </> "bin" </> "test-preprocessor") out
        finish