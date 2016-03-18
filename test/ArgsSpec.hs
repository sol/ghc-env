module ArgsSpec (spec) where

import           Test.Hspec
import           Data.Bifunctor

import           Args

spec :: Spec
spec = do
  describe "parseArgs" $ do
    it "parses GHC version and command" $ do
      (first show $ parseArgs [] ["7.10.2", "bash"]) `shouldBe` ("7.10.2" , ("bash", []))

    context "when command is not specified" $ do
      it "defaults to $SHELL" $ do
        (first show $ parseArgs [("SHELL", "/bin/zsh")] ["7.10.2"]) `shouldBe` ("7.10.2" , ("/bin/zsh", []))

      context "when $SHELL is not defined" $ do
        it "defaults to /bin/sh" $ do
          (first show $ parseArgs [] ["7.10.2"]) `shouldBe` ("7.10.2" , ("/bin/sh", []))

    context "when GHC version is not defined" $ do
      it "defaults to 7.10.3" $ do
        (first show $ parseArgs [] ["bash"]) `shouldBe` ("7.10.3" , ("bash", []))
