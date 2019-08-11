{-# LANGUAGE QuasiQuotes #-}

module Preprocessor where

import SourceExtractor

import Text.Heterocephalus
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Data.ByteString.Lazy.UTF8 hiding (lines, length)

import Data.List

import System.FilePath.Posix
import System.Directory
import System.IO

import Data.Text (strip, pack, unpack)

import Control.Monad

type Verbosity = Int

preprocessDirectory :: String -> FilePath -> FilePath -> IO ()
preprocessDirectory prefix out dir = do
    _ <- putStrLn $ "Scan dir " ++ dir
    fileNames <- getDirectoryContents dir
    entriesNames <- filterM (\input -> return $ input `notElem` [".",".."]) fileNames
    entries <- return $ map (\name -> dir </> name) entriesNames
    fineFiles <- filterM (\input -> doesFileExist input) entries
    fineDirs <- filterM (\input -> doesDirectoryExist input) entries
    _ <- mapM (preprocessDirectory prefix out) fineDirs
    mapM (preprocessFile prefix out) fineFiles >>= \_ -> return ()

preprocessFile :: String -> FilePath -> FilePath -> IO ()
preprocessFile prefix outPath file = do
    z <- putStrLn $ "Open file " ++ file
    name <- return $ takeBaseName file
    specName <- return $ name ++ "Spec"
    specNameFile <- return $ specName ++ ".hs"
    testName <- return $ outPath </> specNameFile
    fileContent <- readFile file
    out <- preprocessTest prefix specName fileContent
    createDirectoryIfMissing True outPath
    writeFile testName out

preprocessTest :: String -> String -> String -> IO String
preprocessTest prefix specName input = do
    (SourceMetadata { testDescription = testDescription, testName = testName }) <- extractTestMetadata input
    multilineStringStart <- return "[r|"
    multilineStringEnd <- return "|]"
    indentedInput <- return $ intercalate "\n" $ filter (\line -> length (unpack $ strip $ pack line) > 0) $ map (\line -> removeAllTags $ "         " ++ (unpack $ strip $ pack line)) $ lines input
    return $ toString $ renderMarkup $
      [compileText|
{-# LANGUAGE QuasiQuotes #-}
module #{prefix}#{specName} where
import Text.RawString.QQ

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

spec = do
  describe "#{specName}: #{testName}" $ do
    it "#{testDescription}" $ do
      c <- extractExecutionErrors $ runWithPrelude 0 "./init/init.ml" #{multilineStringStart}
#{indentedInput}
      #{multilineStringEnd}
      c `shouldBe` Nothing
      |]