module Interpreter.Arithmetic.SimpleSpec (spec) where

import Test.Hspec
import Environment
import Runtime
import Lib

spec :: Spec
spec = do
  describe "Interpreter arithmetics" $ do
    it "Should add two numbers" $ do
      res <- evaluate 2 "2 + 3"
      (getProgramResult res) `shouldBe` (RInt 5)