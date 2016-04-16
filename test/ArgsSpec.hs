module ArgsSpec (spec) where

import           Test.Hspec
import           Data.Bifunctor

import           Args

spec :: Spec
spec = do
  describe "parseArgs" $ do
    it "parses GHC version and command" $ do
      (call_ ["7.10.2", "bash"]) `shouldBe` ("7.10.2" , (Exec "bash" []))

    context "when command is not specified" $ do
      it "defaults to $SHELL" $ do
        (call [("SHELL", "/bin/zsh")] ["7.10.2"]) `shouldBe` ("7.10.2" , Exec "/bin/zsh" [])

      context "when $SHELL is not defined" $ do
        it "defaults to /bin/sh" $ do
          (call_ ["7.10.2"]) `shouldBe` ("7.10.2" , Exec "/bin/sh" [])

    context "when GHC version is not defined" $ do
      it "defaults to 7.10.3" $ do
        (call_ ["bash"]) `shouldBe` ("7.10.3" , Exec "bash" [])

    it "parses --env correctly" $ do
      (call_ ["7.10.2", "--env"]) `shouldBe` ("7.10.2", Source)
  where
    call_ args = call [] args
    call env args = first show $ parseArgs env args
