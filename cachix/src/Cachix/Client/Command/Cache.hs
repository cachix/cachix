module Cachix.Client.Command.Cache (use, remove) where

import Cachix.API qualified as API
import Cachix.Client.Command.Push qualified as Push
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.InstallationMode qualified as InstallationMode
import Cachix.Client.Retry (retryClientM)
import Cachix.Client.Servant
import Protolude hiding (toS)
import Servant.Auth.Client

use :: Env -> Text -> InstallationMode.UseOptions -> IO ()
use env name useOptions = do
  optionalAuthToken <- Config.getAuthTokenMaybe (config env)
  let token = fromMaybe (Token "") optionalAuthToken
  -- 1. get cache public key
  res <- retryClientM (clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> Push.handleCacheResponse name optionalAuthToken err
    Right binaryCache -> do
      nixEnv <- InstallationMode.requireNixEnv
      InstallationMode.addBinaryCache (config env) binaryCache useOptions $
        InstallationMode.getInstallationMode nixEnv useOptions

remove :: Env -> Text -> InstallationMode.UseOptions -> IO ()
remove env name useOptions = do
  nixEnv <- InstallationMode.requireNixEnv
  InstallationMode.removeBinaryCache (Config.hostname $ config env) name $
    InstallationMode.getInstallationMode nixEnv useOptions
