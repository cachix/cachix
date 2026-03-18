module Cachix.Client.Command.DebugInfod (debuginfod) where

import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser (DebugInfodOptions (..))
import Cachix.Client.URI qualified as URI
import Cachix.DebugInfod.Cache qualified as Cache
import Cachix.DebugInfod.Server qualified as Server
import Cachix.DebugInfod.Types (DebugInfodEnv (..))
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Client qualified as Client
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath ((</>))

defaultExpiration :: Int
defaultExpiration = 86400

debuginfod :: Env -> DebugInfodOptions -> IO ()
debuginfod env opts = do
  let cacheName = debugInfodCacheName opts
      port = debugInfodPort opts
      expiration = fromMaybe defaultExpiration (debugInfodExpiration opts)
      host = Config.host (cachixoptions env)
      cacheUrl = URI.appendSubdomain cacheName host
  authToken <- Config.getAuthTokenMaybe (config env)
  let manager = Client.manager (clientenv env)
  cacheDir <- case debugInfodCacheDir opts of
    Just dir -> pure dir
    Nothing -> do
      xdgCache <- getXdgDirectory XdgCache "cachix/debuginfod"
      pure (xdgCache </> toS cacheName)
  fc <- Cache.newFetcherCache (cacheDir </> "debuginfo") expiration
  let debugEnv =
        DebugInfodEnv
          { envCacheBaseUrl = cacheUrl,
            envAuthToken = authToken,
            envHttpManager = manager,
            envClientEnv = clientenv env,
            envCacheName = cacheName,
            envFetcherCache = fc
          }
  Server.runServer debugEnv port
