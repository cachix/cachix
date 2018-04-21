{-# LANGUAGE QuasiQuotes #-}
module Cachix.Client.Commands
  ( authtoken
  , create
  , sync
  , use
  ) where

import Data.String.Here
import Protolude
import Servant.Client

import Cachix.Client.Config        (Config(..), writeConfig, mkConfig)


authtoken :: ClientEnv -> Maybe Config -> Text -> IO ()
authtoken _ maybeConfig token = do
  -- TODO: check that token actually authenticates!
  writeConfig $ case maybeConfig of
    Just config -> config { authToken = token }
    Nothing -> mkConfig token
  putStrLn authTokenNext

create :: ClientEnv -> Maybe Config -> Text -> IO ()
create = undefined

sync :: ClientEnv -> Maybe Config -> Maybe Text -> IO ()
sync = undefined

use :: ClientEnv -> Maybe Config -> Text -> IO ()
use cclient Nothing name = panic "well, you need to authtoken first."
use cclient (Just config) name = do
  -- TODO: 1. get cache public key
  return ()

authTokenNext :: Text
authTokenNext = [hereLit|
Continue by creating a binary cache with:

    $ cachix create <name>
|]
