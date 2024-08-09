module Cachix.Client.Command.Pin (pin) where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.CNix (followLinksToStorePath)
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.OptionsParser (PinOptions (..))
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant
import Cachix.Types.PinCreate qualified as PinCreate
import Data.Text qualified as T
import Hercules.CNix.Store (storePathToPath, withStore)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Client.Streaming
import System.Directory (doesFileExist)

pin :: Env -> PinOptions -> IO ()
pin env pinOpts = do
  authToken <- Config.getAuthTokenRequired (config env)
  storePath <- withStore $ \store -> do
    mpath <- followLinksToStorePath store (encodeUtf8 $ pinStorePath pinOpts)
    maybe exitFailure (storePathToPath store) mpath
  traverse_ (validateArtifact (toS storePath)) (pinArtifacts pinOpts)
  let pinCreate =
        PinCreate.PinCreate
          { name = pinName pinOpts,
            storePath = toS storePath,
            artifacts = pinArtifacts pinOpts,
            keep = pinKeep pinOpts
          }
  void $
    escalate <=< retryHttp $
      (`runClientM` clientenv env) $
        API.createPin cachixClient authToken (pinCacheName pinOpts) pinCreate
  where
    validateArtifact :: Text -> Text -> IO ()
    validateArtifact storePath artifact = do
      -- strip prefix / from artifact path if it exists
      let artifactPath = storePath <> "/" <> fromMaybe artifact (T.stripPrefix "/" artifact)
      exists <- doesFileExist (toS artifactPath)
      unless exists $ throwIO $ ArtifactNotFound $ "Artifact " <> artifactPath <> " doesn't exist."
