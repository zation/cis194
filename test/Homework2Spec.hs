module Homework2Spec (spec) where

import           Homework2
import           Log
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse error" $ parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
    it "should parse information" $ parseMessage  "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
    it "should parse unknown" $ parseMessage  "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
