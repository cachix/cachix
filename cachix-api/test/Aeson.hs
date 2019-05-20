{-# LANGUAGE ScopedTypeVariables #-}
module Aeson where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

checkLaws :: forall a. (ToJSON a, FromJSON a, Eq a) => Gen a -> Spec
checkLaws gen =
  describe "ToJSON/FromJSON laws" $ do
    it "obey partial isomorphism" $
      property $ do
        a <- gen
        pure $ counterexample (T.unpack . T.decodeUtf8 . BL.toStrict . encode $ a) (fromJSON (toJSON a) == Data.Aeson.Success a)
    it "obey toJSON/toEncoding equivalence" $
      property $ do
        a <- gen
        pure $
          counterexample
           (T.unpack . T.decodeUtf8 . BL.toStrict . encode $ a)
           ( Right (toJSON a)
             == eitherDecode (BB.toLazyByteString (fromEncoding (toEncoding a))))
