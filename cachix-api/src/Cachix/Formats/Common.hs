{-# LANGUAGE OverloadedStrings #-}
module Cachix.Formats.Common where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T

withKind :: Text -> (Object -> Parser a) -> Value -> Parser a
withKind k f = withObject (T.unpack k) $ \o -> do
    k' <- o .: "kind"
    when (k' /= k) $ fail $
     "kind field must be " <> show k <> ", not " <> show k'
    f o
