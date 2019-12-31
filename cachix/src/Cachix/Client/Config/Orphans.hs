{-# OPTIONS_GHC -Wno-orphans #-}

module Cachix.Client.Config.Orphans
  (
  )
where

import Dhall hiding (Text)
import Dhall.Core (Chunks (..), Expr (..))
import Protolude
import Servant.Auth.Client

instance FromDhall Token where
  autoWith _ = Decoder
    { extract = ex,
      expected = Text
    }
    where
      ex (TextLit (Chunks [] t)) = pure (Token (toS t))
      ex _ = panic "Unexpected Dhall value. Did it typecheck?"

instance ToDhall Token where
  injectWith _ = Encoder
    { embed = TextLit . Chunks [] . toS . getToken,
      declared = Text
    }
