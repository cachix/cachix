module Cachix.FormatsSpec where

import           Protolude
import           Test.Hspec
import           Test.QuickCheck
import           Cachix.Formats.CachixPullToken
import           Cachix.Formats.CachixSigningKey
import qualified Aeson
import qualified Data.Text                     as T

spec :: Spec
spec = do
  describe "CachixPullToken" $
    Aeson.checkLaws (CachixPullToken <$> text <*> text)
  describe "CachixSigningKey" $
    Aeson.checkLaws (CachixSigningKey <$> text <*> text)

text :: Gen Text
text = T.pack <$> arbitrary
