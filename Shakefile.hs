import Development.Shake
import qualified Development.Shake as Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Shelly (run, liftIO, Sh, shelly, silently, cd, rm_rf, cp)
import Data.Text (pack, unpack)

import Filesystem.Path.CurrentOS (decodeString)
import Text.Regex.TDFA

grepShCommand :: String -> [String] -> String -> Sh (Maybe [String])
grepShCommand command args regex = do
    host <- run (decodeString command) $ map pack args
    matches <- return $ filter (\(_, _, _, m) -> length m > 0) $ map (\line -> (line =~ regex) :: (String, String, String, [String])) $ lines $ unpack host
    result <- return $ case matches of
        ((_, _, _, h:t):_) -> Just $ h:t
        _ -> Nothing
    return result

executeCommand :: String -> [String] -> String -> IO String
executeCommand command args cwd = shelly $ silently $ do
    cd $ decodeString cwd
    result <- run (decodeString command) $ map pack args
    return $ unpack result

executeCommandStack :: [String] -> String -> IO String
executeCommandStack args = executeCommand "stack" (["--allow-different-user"] ++ args)

mainFn :: Sh ()
mainFn = do
    stackPathResult <- grepShCommand "stack" ["path", "--allow-different-user"] "local-install-root: (.*)"
    stackPath <- return $ case stackPathResult of
        Nothing -> ""
        Just (h:_) -> h
    liftIO $ shakeArgs shakeOptions{shakeFiles="_build"} $ do

        want ["_build/interpreter" <.> exe]

        ["parser/*.hs"] &%> \[ouths] -> do
            _ <- liftIO $ executeCommandStack ["exec", "bnfc", "--", "-m", "-o", "parser", "./grammar/syntax.cf"] "."
            _ <- liftIO $ executeCommand "sed" ["-i", "-e", "s/module Main where/module TestSyntax where/g", "parser/TestSyntax.hs"] "."
            _ <- liftIO $ executeCommandStack ["exec", "happy", "--", "-gca", "ParSyntax.y"] "./parser"
            _ <- liftIO $ executeCommandStack ["exec", "alex", "--", "-g", "LexSyntax.x"] "./parser"
            _ <- liftIO $ executeCommand "rm" ["-r", "-f", "-d", "ocamlhs.cabal"] "."
            _ <- liftIO $ executeCommand "rm" ["-r", "-f", "-d", "parser/*.x"] "."
            _ <- liftIO $ executeCommand "rm" ["-r", "-f", "-d", "parser/*.y"] "."
            liftIO $ putStrLn "Done!"

        "_build/interpreter" <.> exe %> \out -> do
            _ <- liftIO $ executeCommandStack ["upgrade", "--binary-version", "2.1.1"] "."
            _ <- liftIO $ executeCommandStack ["install", "--only-dependencies"] "."
            _ <- liftIO $ executeCommandStack ["install", "happy"] "."
            need ["parser/TestSyntax.hs"]
            _ <- liftIO $ executeCommandStack ["build"] "."
            _ <- liftIO $ shelly $ cp (decodeString $ stackPath </> "bin" </> "interpreter") (decodeString "_build/interpreter")
            return ()

main :: IO ()
main = shelly $ silently mainFn