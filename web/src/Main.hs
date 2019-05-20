{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom
import qualified Reflex.Dynamic as Dyn
import qualified Data.Text as T

main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button ">>>"
  let evText = Dyn.tagPromptlyDyn (value t1) evCopy
  t2 <- textInput $ def & setValue .~ evText
  return ()
