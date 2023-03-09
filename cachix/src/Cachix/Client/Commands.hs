{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Commands
  ( authtoken,
    generateKeypair,
    push,
    watchStore,
    watchExec,
    use,
  )
where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.Client.CNix (filterInvalidStorePath)
import qualified Cachix.Client.Config as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.HumanSize (humanSize)
import qualified Cachix.Client.InstallationMode as InstallationMode
import qualified Cachix.Client.NixConf as NixConf
import Cachix.Client.NixVersion (assertNixVersion)
import Cachix.Client.OptionsParser
  ( PushArguments (..),
    PushOptions (..),
  )
import Cachix.Client.Push
import Cachix.Client.Retry (retryAll)
import Cachix.Client.Secrets
  ( SigningKey (SigningKey),
    exportSigningKey,
  )
import Cachix.Client.Servant
import qualified Cachix.Client.WatchStore as WatchStore
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (throwM)
import Control.Retry (RetryStatus (rsIterNumber))
import Crypto.Sign.Ed25519 (PublicKey (PublicKey), createKeypair)
import qualified Data.ByteString.Base64 as B64
import Data.String.Here
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hercules.CNix.Store (Store, StorePath, followLinksToStorePath, storePathToPath, withStore)
import Network.HTTP.Types (status401, status404)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API (NoContent)
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Directory (doesFileExist)
import System.IO (hIsTerminalDevice)
import qualified System.Process

-- TODO: check that token actually authenticates!
authtoken :: Env -> Maybe Text -> IO ()
authtoken Env {cachixoptions} (Just token) = do
  let configPath = Config.configPath cachixoptions
  config <- Config.getConfig configPath
  Config.writeConfig configPath $ config {Config.authToken = Token (toS token)}
authtoken env Nothing = authtoken env . Just . T.strip =<< T.IO.getContents

generateKeypair :: Env -> Text -> IO ()
generateKeypair env name = do
  authToken <- Config.getAuthTokenRequired (config env)
  (PublicKey pk, sk) <- createKeypair
  let signingKey = exportSigningKey $ SigningKey sk
      signingKeyCreate = SigningKeyCreate.SigningKeyCreate (toS $ B64.encode pk)
      bcc = Config.BinaryCacheConfig name signingKey
  -- we first validate if key can be added to the binary cache
  (_ :: NoContent) <-
    escalate <=< retryAll $ \_ ->
      (`runClientM` clientenv env) $
        API.createKey cachixClient authToken name signingKeyCreate
  -- if key was successfully added, write it to the config
  -- TODO: warn if binary cache with the same key already exists
  let cfg = config env & Config.setBinaryCaches [bcc]
  Config.writeConfig (Config.configPath (cachixoptions env)) cfg
  putStrLn
    ( [iTrim|
Secret signing key has been saved in the file above. To populate
your binary cache:

    $ nix-build | cachix push ${name}

Or if you'd like to use the signing key on another machine or CI:

    $ export CACHIX_SIGNING_KEY=${signingKey}
    $ nix-build | cachix push ${name}

To instruct Nix to use the binary cache:

    $ cachix use ${name}

IMPORTANT: Make sure to make a backup for the signing key above, as you have the only copy.
  |] ::
        Text
    )

notAuthenticatedBinaryCache :: Text -> CachixException
notAuthenticatedBinaryCache name =
  AccessDeniedBinaryCache $
    "Binary cache " <> name <> " doesn't exist or it's private and you need a token: " <> Config.noAuthTokenError

accessDeniedBinaryCache :: Text -> Maybe ByteString -> CachixException
accessDeniedBinaryCache name maybeBody =
  AccessDeniedBinaryCache $ "Binary cache " <> name <> " doesn't exist or you don't have access." <> context maybeBody
  where
    context Nothing = ""
    context (Just body) = " Error: " <> toS body

use :: Env -> Text -> InstallationMode.UseOptions -> IO ()
use env name useOptions = do
  optionalAuthToken <- Config.getAuthTokenMaybe (config env)
  let token = fromMaybe (Token "") optionalAuthToken
  -- 1. get cache public key
  res <- retryAll $ \_ -> (`runClientM` clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> handleCacheResponse name optionalAuthToken err
    Right binaryCache -> do
      () <- escalateAs UnsupportedNixVersion =<< assertNixVersion
      user <- InstallationMode.getUser
      nc <- NixConf.read NixConf.Global
      isTrusted <- InstallationMode.isTrustedUser $ NixConf.readLines (catMaybes [nc]) NixConf.isTrustedUsers
      isNixOS <- doesFileExist "/run/current-system/nixos-version"
      let nixEnv =
            InstallationMode.NixEnv
              { InstallationMode.isRoot = user == "root",
                InstallationMode.isTrusted = isTrusted,
                InstallationMode.isNixOS = isNixOS
              }
      InstallationMode.addBinaryCache (config env) binaryCache useOptions $
        InstallationMode.getInstallationMode nixEnv useOptions

handleCacheResponse :: Text -> Maybe Token -> ClientError -> IO a
handleCacheResponse name optionalAuthToken err
  | isErr err status401 && isJust optionalAuthToken = throwM $ accessDeniedBinaryCache name (failureResponseBody err)
  | isErr err status401 = throwM $ notAuthenticatedBinaryCache name
  | isErr err status404 = throwM $ BinaryCacheNotFound $ "Binary cache " <> name <> " does not exist."
  | otherwise = throwM err

failureResponseBody :: ClientError -> Maybe ByteString
failureResponseBody (FailureResponse _ response) = Just $ toS $ responseBody response
failureResponseBody _ = Nothing

push :: Env -> PushArguments -> IO ()
push env (PushPaths opts name cliPaths) = do
  hasStdin <- not <$> hIsTerminalDevice stdin
  inputStorePaths <-
    case (hasStdin, cliPaths) of
      (False, []) -> throwIO $ NoInput "You need to specify store paths either as stdin or as an command argument"
      (True, []) -> T.words <$> getContents
      -- If we get both stdin and cli args, prefer cli args.
      -- This avoids hangs in cases where stdin is non-interactive but unused by caller
      -- some programming environments always create a (non-interactive) stdin
      -- that may or may not be written to by the caller.
      -- This is somewhat like the behavior of `cat` for example.
      (_, paths) -> return paths
  withPushParams env opts name $ \pushParams -> do
    normalized <-
      liftIO $
        for inputStorePaths $
          \path -> do
            storePath <- followLinksToStorePath (pushParamsStore pushParams) (encodeUtf8 path)
            filterInvalidStorePath (pushParamsStore pushParams) storePath
    pushedPaths <-
      pushClosure
        (mapConcurrentlyBounded (numJobs opts))
        pushParams
        (catMaybes normalized)
    case (length normalized, length pushedPaths) of
      (0, _) -> putTextError "Nothing to push."
      (_, 0) -> putTextError "Nothing to push - all store paths are already on Cachix."
      _ -> putTextError "All done."
push _ _ =
  throwIO $
    DeprecatedCommand "DEPRECATED: cachix watch-store has replaced cachix push --watch-store."

putTextError :: Text -> IO ()
putTextError = hPutStrLn stderr

watchStore :: Env -> PushOptions -> Text -> IO ()
watchStore env opts name = do
  withPushParams env opts name $ \pushParams ->
    WatchStore.startWorkers (pushParamsStore pushParams) (numJobs opts) pushParams

watchExec :: Env -> PushOptions -> Text -> Text -> [Text] -> IO ()
watchExec env pushOpts name cmd args = withPushParams env pushOpts name $ \pushParams -> do
  stdoutOriginal <- hDuplicate stdout
  let process =
        (System.Process.proc (toS cmd) (toS <$> args))
          { System.Process.std_out = System.Process.UseHandle stdoutOriginal
          }
      watch = do
        hDuplicateTo stderr stdout -- redirect all stdout to stderr
        WatchStore.startWorkers (pushParamsStore pushParams) (numJobs pushOpts) pushParams

  Async.withAsync watch $ \watchThread -> do
    exitCode <-
      bracketOnError
        (getProcessHandle <$> System.Process.createProcess process)
        ( \processHandle -> do
            -- Terminate the process
            uninterruptibleMask_ (System.Process.terminateProcess processHandle)
            -- Wait for the process to clean up and exit
            _ <- System.Process.waitForProcess processHandle
            -- Stop watching the store and wait for all paths to be pushed
            Async.cancel watchThread
        )
        System.Process.waitForProcess

    Async.cancel watchThread
    exitWith exitCode
  where
    getProcessHandle (_, _, _, processHandle) = processHandle

retryText :: RetryStatus -> Text
retryText retrystatus =
  if rsIterNumber retrystatus == 0
    then ""
    else "(retry #" <> show (rsIterNumber retrystatus) <> ") "

pushStrategy :: Store -> Maybe Token -> PushOptions -> Text -> BinaryCache.CompressionMethod -> StorePath -> PushStrategy IO ()
pushStrategy store authToken opts name compressionMethod storePath =
  PushStrategy
    { onAlreadyPresent = pass,
      on401 = handleCacheResponse name authToken,
      onError = throwM,
      onAttempt = \retrystatus size -> do
        path <- decodeUtf8With lenientDecode <$> storePathToPath store storePath
        -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
        putStr $ retryText retrystatus <> "compressing using " <> T.toLower (show compressionMethod) <> " and pushing " <> path <> " (" <> humanSize (fromIntegral size) <> ")\n",
      onDone = pass,
      Cachix.Client.Push.compressionMethod = compressionMethod,
      Cachix.Client.Push.compressionLevel = Cachix.Client.OptionsParser.compressionLevel opts,
      Cachix.Client.Push.omitDeriver = Cachix.Client.OptionsParser.omitDeriver opts
    }

withPushParams :: Env -> PushOptions -> Text -> (PushParams IO () -> IO ()) -> IO ()
withPushParams env pushOpts name m = do
  pushSecret <- findPushSecret (config env) name
  authToken <- Config.getAuthTokenMaybe (config env)
  compressionMethodBackend <- case pushSecret of
    PushSigningKey {} -> pure Nothing
    PushToken {} -> do
      let token = fromMaybe (Token "") authToken
      res <- retryAll $ \_ -> (`runClientM` clientenv env) $ API.getCache cachixClient token name
      case res of
        Left err -> handleCacheResponse name authToken err
        Right binaryCache -> pure (Just $ BinaryCache.preferredCompressionMethod binaryCache)
  let compressionMethod = fromMaybe BinaryCache.ZSTD (head $ catMaybes [Cachix.Client.OptionsParser.compressionMethod pushOpts, compressionMethodBackend])
  withStore $ \store ->
    m
      PushParams
        { pushParamsName = name,
          pushParamsSecret = pushSecret,
          pushParamsClientEnv = clientenv env,
          pushParamsStrategy = pushStrategy store authToken pushOpts name compressionMethod,
          pushParamsStore = store
        }
