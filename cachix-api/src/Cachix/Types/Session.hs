-- | Auth representations
module Cachix.Types.Session
  ( Session (..),
  )
where

import qualified Crypto.JWT as JWT
import Protolude
import Servant.Auth.JWT (FromJWT (..), ToJWT (..))

data Session
  = JWTSession JWT.ClaimsSet
  deriving (Eq)

instance ToJWT Session where
  encodeJWT (JWTSession s) = s

instance FromJWT Session where
  decodeJWT cs = pure $ JWTSession cs
