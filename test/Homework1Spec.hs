module Homework1Spec (spec) where

import Homework1
import Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should be right" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
