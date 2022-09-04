module DeploySpec where

import Cachix.Types.Deploy
import Data.Aeson
import Data.HashMap.Strict
import Protolude
import Test.Hspec

spec :: Spec
spec =
  describe "Deploy" $ do
    it "parses a simple deploy" testSimple
    it "prases rollbackScript" testRollback

testSimple :: Expectation
testSimple = do
  let input = "{ \"agents\": { \"myagent\": \"/nix/store/blabla\" } }"
  let output =
        Deploy
          { agents = fromList [("myagent", "/nix/store/blabla")],
            rollbackScript = Nothing
          }
  eitherDecode input `shouldBe` Right output

testRollback :: Expectation
testRollback = do
  let input = "{ \"agents\": { \"myagent\": \"/nix/store/blabla\" }, \"rollbackScript\": {\"x86_64-linux\": \"/nix/store/rollback.sh\" }}"
  let output =
        Deploy
          { agents = fromList [("myagent", "/nix/store/blabla")],
            rollbackScript = Just (fromList [("x86_64-linux", "/nix/store/rollback.sh")])
          }
  eitherDecode input `shouldBe` Right output
