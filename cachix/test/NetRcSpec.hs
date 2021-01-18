module NetRcSpec where

import qualified Cachix.Client.NetRc as NetRc
import Cachix.Types.BinaryCache (BinaryCache (..))
import Cachix.Types.Permission (Permission (..))
import Protolude
import Servant.Auth.Client (Token (..))
import System.Directory (copyFile)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

bc1 :: BinaryCache
bc1 =
  BinaryCache
    { name = "name",
      uri = "https://name.cachix.org",
      publicSigningKeys = ["pub"],
      isPublic = False,
      githubUsername = "foobar",
      permission = Read
    }

bc2 :: BinaryCache
bc2 =
  BinaryCache
    { name = "name2",
      uri = "https://name2.cachix.org",
      publicSigningKeys = ["pub2"],
      isPublic = False,
      githubUsername = "foobar2",
      permission = Read
    }

-- TODO: poor man's golden tests, use https://github.com/stackbuilders/hspec-golden
test :: [BinaryCache] -> Text -> Expectation
test caches goldenName = withSystemTempFile "hspec-netrc" $ \filepath _ -> do
  let input = "test/data/" <> toS goldenName <> ".input"
      output = "test/data/" <> toS goldenName <> ".output"
  copyFile input filepath
  NetRc.add (Token "token123") caches filepath
  real <- readFile filepath
  expected <- readFile output
  real `shouldBe` expected

spec :: Spec
spec =
  describe "add" $ do
    -- TODO: not easy to test this with temp files as they are *created*
    --it "populates non-existent netrc file" $ test [bc1, bc2] "fresh"
    it "populates empty netrc file" $ test [bc1, bc2] "empty"
    it "populates netrc file with one additional entry" $ test [bc2] "add"
    it "populates netrc file with one overriden entry" $ test [bc2] "override"
