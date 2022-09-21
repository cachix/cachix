{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Cachix.Client.Config.Orphans
  (
  )
where

import qualified Dhall
import qualified Dhall.Core
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client
import qualified URI.ByteString as URI
import Data.Either.Validation ( Validation(Failure, Success) )

instance Dhall.FromDhall Token where
  autoWith _ = Dhall.strictText {Dhall.extract = ex}
    where
      ex (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) = pure (Token (toS t))
      ex _ = panic "Unexpected Dhall value. Did it typecheck?"

instance Dhall.ToDhall Token where
  injectWith _ = Dhall.Encoder
    { Dhall.embed = Dhall.Core.TextLit . Dhall.Core.Chunks [] . toS . getToken,
      Dhall.declared = Dhall.Core.Text
    }

instance Dhall.FromDhall (URI.URIRef URI.Absolute) where
  autoWith opts =
    Dhall.Decoder extract expected
    where
      textDecoder :: Dhall.Decoder Text
      textDecoder = Dhall.autoWith opts

      extract expression =
        case Dhall.extract textDecoder expression of
          Success x -> case URI.parseURI URI.strictURIParserOptions (toS x) of
            Left exception -> Dhall.extractError (show exception)
            Right path -> Success path
          Failure e -> Failure e

      expected = Dhall.expected textDecoder

instance Dhall.ToDhall (URI.URIRef URI.Absolute) where
  injectWith opts = Dhall.Encoder embed declared
    where
      textEncoder :: Dhall.Encoder Text
      textEncoder = Dhall.injectWith opts

      embed uri = Dhall.embed textEncoder $ toS (URI.serializeURIRef' uri)

      declared = Dhall.Core.Text
