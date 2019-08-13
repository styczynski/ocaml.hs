import Development.Shake
import qualified Development.Shake as Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Shelly (run, liftIO, Sh, shelly, silently, cd, rm_rf, cp)
import Data.Text (pack, unpack)

import System.FilePath.Glob

import Filesystem.Path.CurrentOS (decodeString)
import Text.Regex.TDFA

grepShCommand :: String -> [String] -> String -> String -> Sh (Maybe [String])
grepShCommand command args cwd regex = do
    cd $ decodeString cwd
    host <- run (decodeString command) $ map pack args
    matches <- return $ filter (\(_, _, _, m) -> length m > 0) $ map (\line -> (line =~ regex) :: (String, String, String, [String])) $ lines $ unpack host
    result <- return $ case matches of
        ((_, _, _, h:t):_) -> Just $ h:t
        _ -> Nothing
    return result

getStackInstallDir :: String -> IO String
getStackInstallDir path = shelly $ silently $ do
    stackPathResult <- grepShCommand "stack" ["path", "--allow-different-user"] path "local-install-root: (.*)"
    stackPath <- return $ case stackPathResult of
        Nothing -> ""
        Just (h:_) -> h
    return stackPath

executeCommand :: String -> [String] -> String -> IO String
executeCommand command args cwd = shelly $ do
    cd $ decodeString cwd
    result <- run (decodeString command) $ map pack args
    return $ unpack result

executeCommandStack :: [String] -> String -> IO String
executeCommandStack args = executeCommand "stack" (["--allow-different-user"] ++ args)

mainFn :: Sh ()
mainFn = do

    liftIO $ shakeArgs shakeOptions{shakeFiles="_build"} $ do

        want ["_build/test-preprocessor" <.> exe]

        ["parser/*.hs"] &%> \[ouths] -> do
            _ <- liftIO $ executeCommandStack ["exec", "bnfc", "--", "-m", "-o", "parser", "./grammar/syntax.cf"] "."
            _ <- liftIO $ executeCommand "sed" ["-i", "-e", "s/module Main where/module TestSyntax where/g", "parser/TestSyntax.hs"] "."
            _ <- liftIO $ executeCommandStack ["exec", "happy", "--", "-gca", "ParSyntax.y"] "./parser"
            _ <- liftIO $ executeCommandStack ["exec", "alex", "--", "-g", "LexSyntax.x"] "./parser"
            filesXToRemove <- liftIO $ glob "parser/*.x"
            filesYToRemove <- liftIO $ glob "parser/*.y"
            filesCabalToRemove <- liftIO $ glob "ocamlhs.cabal"
            _ <- liftIO $ executeCommand "rm" (["-r", "-f", "-d"] ++ filesXToRemove ++ filesYToRemove ++ filesCabalToRemove) "."
            liftIO $ putStrLn "Done!"

        "_build/interpreter" <.> exe %> \out -> do
            stackPath <- liftIO $ getStackInstallDir "."
            _ <- liftIO $ executeCommandStack ["upgrade", "--binary-version", "2.1.1"] "."
            _ <- liftIO $ executeCommandStack ["install", "--only-dependencies"] "."
            _ <- liftIO $ executeCommandStack ["install", "happy"] "."
            need ["parser/TestSyntax.hs"]
            srcFiles <- liftIO $ glob "src/**/*.hs"
            appFiles <- liftIO $ glob "app/**/*.hs"
            grammarFiles <- liftIO $ glob "grammar/**/*.cf"
            need $ srcFiles ++ appFiles ++ grammarFiles
            _ <- liftIO $ executeCommandStack ["build"] "."
            _ <- liftIO $ shelly $ cp (decodeString $ stackPath </> "bin" </> "interpreter") (decodeString out)
            return ()

        "_build/test-preprocessor" <.> exe %> \out -> do
            preprocessorStackPath <- liftIO $ getStackInstallDir "test-preprocessor"
            _ <- liftIO $ executeCommandStack ["build"] "test-preprocessor"
            _ <- liftIO $ shelly $ cp (decodeString $ preprocessorStackPath </> "bin" </> "test-preprocessor") (decodeString out)
            return ()

main :: IO ()
main = shelly $ silently mainFn