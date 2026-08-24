module TerminalSpec (spec) where

import Cachix.Client.Terminal (isCI, useInteractiveProgress)
import Protolude
import System.IO (IOMode (WriteMode), withFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "isCI" $ do
    it "recognizes supported CI values" $ do
      fmap isCI [Just "true", Just "1", Just "TRUE"] `shouldBe` [True, True, True]

    it "does not treat other values as CI" $ do
      fmap isCI [Nothing, Just "false", Just "0", Just ""] `shouldBe` [False, False, False, False]

  describe "useInteractiveProgress" $ do
    it "rejects non-terminal handles" $ do
      withFile "/dev/null" WriteMode $ \handle ->
        useInteractiveProgress handle `shouldReturn` False
