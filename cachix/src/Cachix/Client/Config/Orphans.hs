{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module Cachix.Client.Config.Orphans
  (
  )
where

import Dhall hiding (Text)
import Dhall.Core (Chunks (..), Expr (..))
import Protolude
import Servant.Auth.Client

#if MIN_VERSION_dhall(1,28,0)
instance FromDhall Token where
  autoWith _ = Decoder
#else
instance Interpret Token where
  autoWith _ = Type
#endif
    { extract = ex,
      expected = Text
    }
    where
      ex (TextLit (Chunks [] t)) = pure (Token (toS t))
      ex _ = panic "Unexpected Dhall value. Did it typecheck?"

#if MIN_VERSION_dhall(1,28,0)
instance ToDhall Token where
  injectWith _ = Encoder
#else
instance Inject Token where
  injectWith _ = InputType
#endif
    { embed = TextLit . Chunks [] . toS . getToken,
      declared = Text
    }
