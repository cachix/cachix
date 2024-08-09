module Cachix.Client.Command.Cache (use, remove) where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.Command.Push qualified as Push
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.InstallationMode qualified as InstallationMode
import Cachix.Client.NixVersion (assertNixVersion)
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant
import Protolude hiding (toS)
import Servant.Auth.Client
import Servant.Client.Streaming

use :: Env -> Text -> InstallationMode.UseOptions -> IO ()
use env name useOptions = do
  optionalAuthToken <- Config.getAuthTokenMaybe (config env)
  let token = fromMaybe (Token "") optionalAuthToken
  -- 1. get cache public key
  res <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> Push.handleCacheResponse name optionalAuthToken err
    Right binaryCache -> do
      () <- escalateAs UnsupportedNixVersion =<< assertNixVersion
      nixEnv <- InstallationMode.getNixEnv
      InstallationMode.addBinaryCache (config env) binaryCache useOptions $
        InstallationMode.getInstallationMode nixEnv useOptions

remove :: Env -> Text -> IO ()
remove env name = do
  nixEnv <- InstallationMode.getNixEnv
  InstallationMode.removeBinaryCache (Config.hostname $ config env) name $
    InstallationMode.getInstallationMode nixEnv InstallationMode.defaultUseOptions
