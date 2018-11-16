module Homework1Spec (spec) where

import           Homework1
import           Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should be right" $ toDigits 1234 `shouldBe` [1,2,3,4]
    it "should handle 0" $ toDigits 0 `shouldBe` []
  describe "toDigitsRev" $ do
    it "should be right" $ toDigitsRev 1234 `shouldBe` [4,3,2,1]
    it "should handle 0" $ toDigits 0 `shouldBe` []
  describe "doubleEveryOther" $ do
    it "should double odd array" $ doubleEveryOther [1,2,3,4] `shouldBe` [2,2,6,4]
    it "should double even array" $ doubleEveryOther [1,2,3,4,5] `shouldBe` [1,4,3,8,5]
    it "should not double empty array" $ doubleEveryOther [] `shouldBe` []
  describe "sumDigits" $ do
    it "should sum by digit" $ sumDigits [16,1,22,3] `shouldBe` 15
    it "should sum empty array as 0" $ sumDigits [] `shouldBe` 0
  describe "validate" $ do
    it "should be right" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False
  describe "hanoi" $ do
    it "should calculate steps" $ do
      hanoi 3 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
