{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module for auth session storage
module Cachix.Types.Session
  ( Session (..),
    UserId
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Protolude
-- TODO: move these two into Servant.Auth
import Servant.Auth.Server (FromJWT, ToJWT)

type UserId = Integer

newtype Session
  = Session UserId
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)
