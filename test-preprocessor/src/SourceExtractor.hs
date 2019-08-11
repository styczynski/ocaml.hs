module SourceExtractor where

import Text.Regex.TDFA
import Data.Text (strip, pack, unpack)
import Data.Array (elems)

regexTagTest = "\\(\\*\\* *Test: +(.+) +\\*\\*\\)"
regexTagDesribe = "\\(\\*\\* *Describe: +(.+) +\\*\\*\\)"

data SourceMetadata = SourceMetadata
  { testDescription :: String, testName :: String }

replaceAll :: String -> String -> String -> String
replaceAll regex new_str str  =
    let parts = concat $ map elems $ (str  =~  regex :: [MatchArray])
    in foldl (replace' new_str) str (reverse parts)

  where
     replace' :: [a] -> [a] -> (Int, Int) -> [a]
     replace' new list (shift, l)   =
        let (pre, post) = splitAt shift list
        in pre ++ new ++ (drop l post)

removeAllTags :: String -> String
removeAllTags input = replaceAll "\\(\\*\\* *(.+): +(.+) +\\*\\*\\)" "" input

extractTagUsing :: String -> SourceMetadata -> String -> (SourceMetadata -> [String] -> SourceMetadata) -> IO SourceMetadata
extractTagUsing input meta regex foldFn = do
    (a, b, c, descrMatchGroups) <- return $ ((input =~ regex) :: (String, String, String, [String]))
    return $ foldl foldFn meta [descrMatchGroups]

extractTagDescriptionFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagDescriptionFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { testDescription = unpack $ strip $ pack h }

extractTagTestFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagTestFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { testName = unpack $ strip $ pack h }

extractTestMetadata :: String -> IO SourceMetadata
extractTestMetadata input = do
    initMeta <- return $ SourceMetadata { testDescription = "", testName = "" }
    meta0 <- extractTagUsing input initMeta regexTagDesribe extractTagDescriptionFn
    meta1 <- extractTagUsing input meta0 regexTagTest extractTagTestFn
    return meta1