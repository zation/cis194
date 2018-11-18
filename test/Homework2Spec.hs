module Homework2Spec (spec) where

import           Homework2
import           Log
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse error" $ parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
    it "should parse information" $ parseMessage  "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
    it "should parse warning" $ parseMessage  "W 11 bong bong" `shouldBe` LogMessage Warning 11 "bong bong"
    it "should parse unknown" $ parseMessage  "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
  describe "parse" $ do
    it "should parse multiple lines" $ do
      parse "E 2 562 help help\nI 29 la la la\nW 11 bong bong\nThis is not in the right format" `shouldBe`
        [LogMessage (Error 2) 562 "help help", LogMessage Info 29 "la la la", LogMessage Warning 11 "bong bong", Unknown "This is not in the right format"]
