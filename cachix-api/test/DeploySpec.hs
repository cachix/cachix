module DeploySpec where

import Cachix.Types.Deploy
import Data.Aeson
import Data.HashMap.Strict
import Protolude
import Test.Hspec

spec :: Spec
spec =
  describe "Deploy" $ do
    it "parses a simple deploy" test

test :: Expectation
test = do
  let input = "{ \"agents\": { \"myagent\": \"/nix/store/blabla\" } }"
  let output = Deploy {agents = fromList [("myagent", "/nix/store/blabla")], rollbackScript = Nothing}
  eitherDecode input `shouldBe` Right output
