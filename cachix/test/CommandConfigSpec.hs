module CommandConfigSpec (spec) where

import Cachix.Client.Command.Config (generateKeypairAccessDenied)
import Protolude
import Test.Hspec

spec :: Spec
spec =
  describe "generateKeypairAccessDenied" $ do
    it "explains that signing-key creation requires a personal administrator token" $ do
      let message = displayException $ generateKeypairAccessDenied "corestory"
      message `shouldContain` "Cannot create a signing key for binary cache corestory."
      message `shouldContain` "requires a personal auth token"
      message `shouldContain` "Per-cache read/write tokens"
