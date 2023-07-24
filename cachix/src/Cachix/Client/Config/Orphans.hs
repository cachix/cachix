{-# OPTIONS_GHC -Wno-orphans #-}

module Cachix.Client.Config.Orphans where

import qualified Dhall
import qualified Dhall.Core
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client

instance Dhall.FromDhall Token where
  autoWith _ = Dhall.strictText {Dhall.extract = ex}
    where
      ex (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) = pure (Token (toS t))
      ex _ = panic "Unexpected Dhall value. Did it typecheck?"

instance Dhall.ToDhall Token where
  injectWith _ =
    Dhall.Encoder
      { Dhall.embed = Dhall.Core.TextLit . Dhall.Core.Chunks [] . toS . getToken,
        Dhall.declared = Dhall.Core.Text
      }
