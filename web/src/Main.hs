{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Reflex.Dom
import qualified Reflex.Dynamic as Dyn
import qualified Data.Text as T
import System.IO.Unsafe

main :: IO ()
main = mainWidget body

fn e = do
  val <- return $ T.unpack e
  return $ T.pack $ (unsafePerformIO $ run 2 val)

body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button ">>>"
  let evText = Dyn.tagPromptlyDyn ((value t1) >>= fn) evCopy
  t2 <- textInput $ def & setValue .~ evText
  return ()
