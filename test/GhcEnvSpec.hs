module GhcEnvSpec (spec) where

import           Test.Hspec

import           GhcEnv

spec :: Spec
spec = do
  describe "modifyPath" $ do
    context "without an existing PATH" $ do
      it "creates PATH" $ do
        modifyPath ["/foo", "/bar"] Nothing `shouldBe` "/foo:/bar"

    context "with an existing PATH" $ do
      it "modifies PATH" $ do
        modifyPath ["/foo", "/bar"] (Just "/bin") `shouldBe` "/foo:/bar:/bin"

     
