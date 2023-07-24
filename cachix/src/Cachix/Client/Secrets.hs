-- A facade for working with secrets and their representations, without delving
-- into cryptography libraries.
module Cachix.Client.Secrets
  ( -- * NAR signing
    SigningKey (..),
    parseSigningKeyLenient,
    exportSigningKey,
  )
where

-- TODO: * Auth token
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Protolude hiding (toS)
import Protolude.Conv

-- | A secret key for signing nars.
newtype SigningKey = SigningKey {signingSecretKey :: SecretKey}

parseSigningKeyLenientBS ::
  -- | ASCII (Base64)
  ByteString ->
  -- | Error message or signing key
  Either Text SigningKey
parseSigningKeyLenientBS raw =
  let bcDropWhileEnd f = BC.reverse . BC.dropWhile f . BC.reverse
      bcDropAround f = bcDropWhileEnd f . BC.dropWhile f
      stripped = bcDropAround isSpace raw
      nonNull = if BC.null stripped then Left "A signing key must not be empty" else pure stripped
   in SigningKey . SecretKey . B64.decodeLenient <$> nonNull

parseSigningKeyLenient ::
  -- | Base64
  Text ->
  -- | Error message or signing key
  Either Text SigningKey
parseSigningKeyLenient = parseSigningKeyLenientBS . toSL

exportSigningKeyBS ::
  SigningKey ->
  -- | ASCII (Base64)
  ByteString
exportSigningKeyBS (SigningKey (SecretKey bs)) = B64.encode bs

exportSigningKey :: SigningKey -> Text
exportSigningKey = toS . exportSigningKeyBS
