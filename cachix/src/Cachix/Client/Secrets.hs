-- A facade for working with secrets and their representations, without delving
-- into cryptography libraries.
module Cachix.Client.Secrets
  ( -- * NAR signing
    SigningKey (..),
    parseSigningKeyLenient,
    exportSigningKey
    )
where

-- TODO: * Auth token
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Protolude

-- | A secret key for signing nars.
newtype SigningKey = SigningKey {signingSecretKey :: SecretKey}

parseSigningKeyLenientBS
  :: ByteString -- ^ ASCII (Base64)
  -> Either Text SigningKey -- ^ Error message or signing key
parseSigningKeyLenientBS raw =
  let bcDropWhileEnd f = BC.reverse . BC.dropWhile f . BC.reverse
      bcDropAround f = bcDropWhileEnd f . BC.dropWhile f
      stripped = bcDropAround isSpace raw
      nonNull = if BC.null stripped then Left "A signing key must not be empty" else pure stripped
   in SigningKey . SecretKey . B64.decodeLenient <$> nonNull

parseSigningKeyLenient
  :: Text -- ^ Base64
  -> Either Text SigningKey -- ^ Error message or signing key
parseSigningKeyLenient = parseSigningKeyLenientBS . toSL

exportSigningKeyBS
  :: SigningKey
  -> ByteString -- ^ ASCII (Base64)
exportSigningKeyBS (SigningKey (SecretKey bs)) = B64.encode bs

exportSigningKey :: SigningKey -> Text
exportSigningKey = toS . exportSigningKeyBS
