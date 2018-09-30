-- | Module for auth session storage
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Cachix.Types.Session
  ( Session(..)
  , UserId
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

type UserId = Integer

newtype Session =
  Session UserId
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
