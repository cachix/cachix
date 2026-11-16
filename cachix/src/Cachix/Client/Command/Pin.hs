module Cachix.Client.Command.Pin (pin, getPin) where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.CNix (formatStorePathWarning, resolveStorePath)
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.OptionsParser (PinGetOptions (..), PinOptions (..))
import Cachix.Client.Retry (retryClientM)
import Cachix.Client.Servant
import Cachix.Types.Pin qualified as Pin
import Cachix.Types.PinCreate qualified as PinCreate
import Cachix.Types.PinRevision qualified as PinRevision
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as T
import Hercules.CNix.Store (storePathToPath, withStore)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token (..))
import System.Directory (doesFileExist)

pin :: Env -> PinOptions -> IO ()
pin env pinOpts = do
  authToken <- Config.getAuthTokenRequired (config env)
  storePath <- withStore $ \store -> do
    let filePath = toS (pinStorePath pinOpts)
    resolveStorePath store filePath >>= \case
      Left err -> do
        putErrText $ formatStorePathWarning filePath err
        exitFailure
      Right sp -> storePathToPath store sp
  traverse_ (validateArtifact (toS storePath)) (pinArtifacts pinOpts)
  let pinCreate =
        PinCreate.PinCreate
          { name = pinName pinOpts,
            storePath = toS storePath,
            artifacts = pinArtifacts pinOpts,
            keep = pinKeep pinOpts
          }
  void $
    escalate
      =<< retryClientM (clientenv env) (API.createPin cachixClient authToken (pinCacheName pinOpts) pinCreate)
  where
    validateArtifact :: Text -> Text -> IO ()
    validateArtifact storePath artifact = do
      -- strip prefix / from artifact path if it exists
      let artifactPath = storePath <> "/" <> fromMaybe artifact (T.stripPrefix "/" artifact)
      exists <- doesFileExist (toS artifactPath)
      unless exists $ throwIO $ ArtifactNotFound $ "Artifact " <> artifactPath <> " doesn't exist."

getPin :: Env -> PinGetOptions -> IO ()
getPin env opts = do
  token <- fromMaybe (Token "") <$> Config.getAuthTokenMaybe (config env)
  pins <- escalate =<< retryClientM (clientenv env) (listPins token (pinGetCacheName opts))
  result <- case find ((== pinGetName opts) . Pin.name) pins of
    Just info -> pure info
    Nothing -> throwIO $ PinNotFound $ "Pin " <> pinGetName opts <> " was not found in cache " <> pinGetCacheName opts <> "."
  if pinGetJson opts
    then LBS.putStrLn $ Aeson.encode result
    else putText $ renderPin result

renderPin :: Pin.Pin -> Text
renderPin info =
  T.intercalate "\n" $
    [ "Name: " <> Pin.name info,
      "Created: " <> show (Pin.createdOn info),
      "Retention: " <> renderKeep (Pin.keep info),
      "Store path: " <> PinRevision.storePath latest,
      "Revision: " <> show (PinRevision.revision latest),
      "Revision created: " <> show (PinRevision.createdOn latest),
      "Orphaned: " <> if PinRevision.orphaned latest then "yes" else "no"
    ]
      <> ["Artifact: " <> artifact | artifact <- PinRevision.artifacts latest]
  where
    latest = Pin.lastRevision info
    renderKeep = \case
      PinCreate.Days days -> "Revisions from the last " <> show days <> " days"
      PinCreate.Revisions count -> "Last " <> show count <> " revisions"
      PinCreate.Forever -> "Forever"
