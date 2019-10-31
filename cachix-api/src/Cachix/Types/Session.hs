-- | Auth representations
module Cachix.Types.Session
  ( Session (..),
  )
where

import Protolude
import Servant.Auth.Server (FromJWT(..), ToJWT(..))
import qualified Crypto.JWT as JWT

data Session
  = JWTSession JWT.ClaimsSet
  deriving Eq

instance ToJWT Session where
  encodeJWT (JWTSession s) = s
instance FromJWT Session where
  decodeJWT cs = pure $ JWTSession cs
