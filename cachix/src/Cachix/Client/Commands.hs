{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Cachix.Client.Commands
  ( authtoken
  , create
  , sync
  , use
  ) where

import           Crypto.Sign.Ed25519
import           Data.String.Here
import           Protolude
import           Servant.API
import           Servant.Client
import           Servant.Auth
import           Servant.Auth.Client
import           Servant.Streaming.Client
import           Servant.Generic
import           System.Environment             ( lookupEnv )
import           Web.Cookie                     ( SetCookie )


import qualified Cachix.Api                    as Api
import           Cachix.Client.Config           ( Config(..)
                                                , BinaryCacheConfig(..)
                                                , writeConfig
                                                , mkConfig
                                                )
import           Cachix.Client.NixVersion       ( getNixMode
                                                , NixMode(..)
                                                )
import qualified Cachix.Client.NixConf         as NixConf
import           Cachix.Client.Servant


cachixClient :: Api.CachixAPI AsClient
cachixClient = fromServant $ client Api.servantApi

cachixBCClient :: Text -> Api.BinaryCacheAPI AsClient
cachixBCClient name = fromServant $ Api.cache cachixClient name

authtoken :: ClientEnv -> Maybe Config -> Text -> IO ()
authtoken _ maybeConfig token = do
  -- TODO: check that token actually authenticates!
  writeConfig $ case maybeConfig of
    Just config -> config { authToken = token }
    Nothing -> mkConfig token
  putStrLn ([hereLit|
  Continue by creating a binary cache with:

      $ cachix create <name>
  |] :: Text)

create :: ClientEnv -> Maybe Config -> Text -> IO ()
create _ Nothing _ = panic "well, you need to authtoken first."
create env (Just config@Config{..}) name = do
  (PublicKey pk, SecretKey sk) <- createKeypair

  let bc = Api.BinaryCacheCreate $ toS pk
  res <- runClientM (Api.create (cachixBCClient name) (Token (toS authToken)) bc) env
  case res of
    -- TODO: handle all kinds of errors
    Left err -> putStrLn $ "API Error: " ++ show err
    Right _ -> do
      -- write signing key to config
      let bcc = BinaryCacheConfig name (toS sk)
      writeConfig $ config { binaryCaches = binaryCaches <> [bcc] }

      putStrLn ([hereLit|
      -- TODO: tell how to use signing key and public cache
      |] :: Text)

use :: ClientEnv -> Maybe Config -> Text -> IO ()
use env _ name = do
  -- 1. get cache public key
  res <- runClientM (Api.get (cachixBCClient name)) env
  case res of
    -- TODO: handle 404
    Left err -> putStrLn $ "API Error: " ++ show err
    Right Api.BinaryCache{..} -> do
      -- 2. Detect Nix version
      nixMode <- getNixMode
      case nixMode of
        Left err -> panic err
        Right Nix1XX -> panic "Please upgrade to Nix 2.0.1"
         -- TODO: require sudo, use old naming
        Right Nix20 -> panic "Please upgrade to Nix 2.0.1"
         -- TODO: If Nix 2.0, skip trusted user check, require sudo
         -- TODO: NixOS, abort :(
         {-
         maybeUser <- lookupEnv "USER"
         if isRoot (toS <$> maybeUser)
         then return ()
         else return ()
         -}
        Right Nix201 -> do
           -- 2. Detect if is a trusted user (or running single mode?)
           nc <- NixConf.read NixConf.Global
           if isTrustedUser nc
           -- If is a trusted user, populate user nix.conf
           then NixConf.update NixConf.Local $ addSubstituters name publicSigningKeys
           -- if not, tell user what to do (TODO: require sudo to do it)
           else panic "TODO"

fqdn :: Text -> Text
fqdn name = name <> ".cachix.org"

addSubstituters :: Text -> [Text] -> Maybe NixConf.NixConf -> NixConf.NixConf
addSubstituters name pks (Just nixconf) = undefined -- TODO
addSubstituters name pks Nothing =
  NixConf.NixConf
    [ NixConf.Substituters ["https://" <> fqdn name]
    , NixConf.TrustedPublicKeys (fmap (\pk-> fqdn name <> "-1:" <> pk) pks)
    ]

-- TODO: well, define this one
isTrustedUser x = True

isRoot :: Maybe Text -> Bool
isRoot Nothing = panic "$USER is not set"
isRoot (Just user) = user == "root"

sync :: ClientEnv -> Maybe Config -> Maybe Text -> IO ()
sync = undefined
  -- TODO: query list of paths
  -- TODO: stream nar
  -- TODO: upload narinfo with signing
