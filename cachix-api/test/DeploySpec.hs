module DeploySpec where

import Cachix.Types.Deploy
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict
import Protolude
import Test.Hspec
import qualified Prelude

spec :: Spec
spec =
  describe "Deploy" $ do
    it "parses a simple deploy" testSimple
    it "parses rollbackScript" testRollback

test :: BSL.ByteString -> Expectation
test input = (encode <$> (eitherDecode input :: Either Prelude.String Deploy)) `shouldBe` Right input

testSimple :: Expectation
testSimple = do
  let input = "{\"agents\":{\"myagent\":\"/nix/store/blabla\"}}"
  test input

testRollback :: Expectation
testRollback = do
  let input = "{\"agents\":{\"myagent\":\"/nix/store/blabla\"},\"rollbackScript\":{\"x86_64-linux\":\"/nix/store/rollback.sh\"}}"
  test input
