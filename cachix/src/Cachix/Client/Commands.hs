{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Commands
  ( authtoken,
    generateKeypair,
    push,
    watchStore,
    watchExec,
    watchExecDaemon,
    watchExecStore,
    use,
    import',
    remove,
    pin,
  )
where

import qualified Amazonka
import Amazonka.Data.Body (ResponseBody (..))
import qualified Amazonka.Data.Text
import qualified Amazonka.S3
import Amazonka.S3.GetObject (getObjectResponse_body)
import Amazonka.S3.ListObjectsV2 (listObjectsV2Response_contents)
import Amazonka.S3.Types.Object (object_key)
import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import Cachix.Client.Commands.Push
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Daemon as Daemon
import qualified Cachix.Client.Daemon.PostBuildHook as Daemon.PostBuildHook
import qualified Cachix.Client.Daemon.Progress as Daemon.Progress
import Cachix.Client.Daemon.Types
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import qualified Cachix.Client.InstallationMode as InstallationMode
import qualified Cachix.Client.NixConf as NixConf
import Cachix.Client.NixVersion (assertNixVersion)
import Cachix.Client.OptionsParser
  ( DaemonOptions (..),
    PinOptions (..),
    PushArguments (..),
    PushOptions (..),
  )
import Cachix.Client.Push
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Secrets
  ( SigningKey (SigningKey),
    exportSigningKey,
  )
import Cachix.Client.Servant
import Cachix.Client.URI (URI)
import qualified Cachix.Client.URI as URI
import qualified Cachix.Client.WatchStore as WatchStore
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.PinCreate as PinCreate
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import Conduit
import qualified Control.Concurrent.Async as Async
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Retry (defaultRetryStatus)
import Crypto.Sign.Ed25519 (PublicKey (PublicKey), createKeypair)
import qualified Data.Attoparsec.Text
import qualified Data.ByteString.Base64 as B64
import qualified Data.Conduit.Combinators as C
import Data.Conduit.ConcurrentMap (concurrentMapM_)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as C
import Data.Generics.Labels ()
import Data.HashMap.Strict as HashMap
import Data.IORef
import Data.String.Here
import qualified Data.Text as T
import Data.Text.IO (hGetLine)
import qualified Data.Text.IO as T.IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hercules.CNix.Store (parseStorePath, storePathToPath, withStore)
import Lens.Micro
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status404)
import qualified Nix.NarInfo as NarInfo
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API (NoContent (..))
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Console.Pretty
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.IO (hIsTerminalDevice)
import System.IO.Error (isEOFError)
import System.IO.Temp (withTempFile)
import qualified System.Posix.Signals as Signals
import qualified System.Process
import qualified URI.ByteString as UBS

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
    escalate <=< retryHttp $
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

getNixEnv :: IO InstallationMode.NixEnv
getNixEnv = do
  user <- InstallationMode.getUser
  nc <- NixConf.read NixConf.Global
  isTrusted <- InstallationMode.isTrustedUser $ NixConf.readLines (catMaybes [nc]) NixConf.isTrustedUsers
  isNixOS <- doesFileExist "/run/current-system/nixos-version"
  return $
    InstallationMode.NixEnv
      { InstallationMode.isRoot = user == "root",
        InstallationMode.isTrusted = isTrusted,
        InstallationMode.isNixOS = isNixOS
      }

use :: Env -> Text -> InstallationMode.UseOptions -> IO ()
use env name useOptions = do
  optionalAuthToken <- Config.getAuthTokenMaybe (config env)
  let token = fromMaybe (Token "") optionalAuthToken
  -- 1. get cache public key
  res <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> handleCacheResponse name optionalAuthToken err
    Right binaryCache -> do
      () <- escalateAs UnsupportedNixVersion =<< assertNixVersion
      nixEnv <- getNixEnv
      InstallationMode.addBinaryCache (config env) binaryCache useOptions $
        InstallationMode.getInstallationMode nixEnv useOptions

remove :: Env -> Text -> IO ()
remove env name = do
  nixEnv <- getNixEnv
  InstallationMode.removeBinaryCache (Config.hostname $ config env) name $
    InstallationMode.getInstallationMode nixEnv InstallationMode.defaultUseOptions

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
    normalized <- liftIO $
      for inputStorePaths $ \path ->
        runMaybeT $ do
          storePath <- MaybeT $ followLinksToStorePath (pushParamsStore pushParams) (encodeUtf8 path)
          MaybeT $ filterInvalidStorePath (pushParamsStore pushParams) storePath
    pushedPaths <-
      pushClosure
        (mapConcurrentlyBounded (numJobs opts))
        pushParams
        (catMaybes normalized)
    case (length normalized, length pushedPaths) of
      (0, _) -> putErrText "Nothing to push."
      (_, 0) -> putErrText "Nothing to push - all store paths are already on Cachix."
      _ -> putErrText "\nAll done."
push _ _ =
  throwIO $
    DeprecatedCommand "DEPRECATED: cachix watch-store has replaced cachix push --watch-store."

discoverAwsEnv :: Maybe ByteString -> Maybe ByteString -> IO Amazonka.Env
discoverAwsEnv maybeEndpoint maybeRegion = do
  s3Endpoint <-
    case maybeEndpoint of
      Nothing -> pure Amazonka.S3.defaultService
      Just url -> do
        req <- HTTP.parseRequest (toS url)
        pure $
          Amazonka.S3.defaultService
            & Amazonka.setEndpoint (HTTP.secure req) (HTTP.host req) (HTTP.port req)
            -- Don't overwrite requests into the virtual-hosted style i.e. <bucket>.<host>.
            -- This would break IP-based endpoints, like our test endpoint.
            & #s3AddressingStyle .~ Amazonka.S3AddressingStylePath

  region <-
    traverse (escalateAs (FatalError . toS) . Amazonka.Data.Text.fromText . toS) maybeRegion

  -- Create a service client with the custom endpoint
  Amazonka.newEnv Amazonka.discover
    <&> Amazonka.configureService s3Endpoint
      . maybe identity (#region .~) region

import' :: Env -> PushOptions -> Text -> URI -> IO ()
import' env pushOptions name s3uri = do
  awsEnv <- discoverAwsEnv (URI.getQueryParam s3uri "endpoint") (URI.getQueryParam s3uri "region")
  putErrText $ "Importing narinfos/nars using " <> show (numJobs pushOptions) <> " workers from " <> URI.serialize s3uri <> " to " <> name
  putErrText ""
  Amazonka.runResourceT $
    runConduit $
      Amazonka.paginate awsEnv (Amazonka.S3.newListObjectsV2 bucketName)
        .| CL.mapMaybe (^. listObjectsV2Response_contents)
        .| CL.concat
        .| CL.map (^. object_key)
        .| CL.filter (T.isSuffixOf ".narinfo" . Amazonka.Data.Text.toText)
        .| concurrentMapM_ (numJobs pushOptions) (numJobs pushOptions * 2) (uploadNarinfo awsEnv)
        .| CL.sinkNull
  putErrText "All done."
  where
    bucketName :: Amazonka.S3.BucketName
    bucketName = Amazonka.S3.BucketName bucketNameText
    bucketNameText = toS $ UBS.hostBS $ URI.getHostname s3uri

    getObject ::
      (MonadResource m) =>
      Amazonka.Env ->
      Amazonka.S3.ObjectKey ->
      m (ConduitT () ByteString (ResourceT IO) ())
    getObject awsEnv key = do
      rs <- Amazonka.send awsEnv (Amazonka.S3.newGetObject bucketName key)
      return $ body $ rs ^. getObjectResponse_body

    fileHashParse :: Text -> IO Text
    fileHashParse s
      | "sha256:" `T.isPrefixOf` s = return $ T.drop 7 s
      | otherwise = throwM $ ImportUnsupportedHash $ "file hash " <> s <> " is unsupported. Leave us feedback at https://github.com/cachix/cachix/issues/601"

    uploadNarinfo :: Amazonka.Env -> Amazonka.S3.ObjectKey -> ResourceT IO ()
    uploadNarinfo awsEnv entry = liftIO $ do
      let storeHash = T.dropEnd 8 $ Amazonka.Data.Text.toText entry

      -- get narinfo
      narinfoText <- runConduitRes $ do
        narinfoStream <- getObject awsEnv entry
        narinfoStream .| C.decodeUtf8 .| C.fold

      -- parse narinfo
      case Data.Attoparsec.Text.parseOnly NarInfo.parseNarInfo (toS narinfoText) of
        Left e -> hPutStr stderr $ "error while parsing " <> storeHash <> ": " <> show e
        Right parsedNarInfo -> do
          -- we support only sha256: for now
          narInfo <- do
            fileHash <- fileHashParse $ NarInfo.fileHash parsedNarInfo
            return $ parsedNarInfo {NarInfo.fileHash = fileHash}

          -- stream nar and narinfo
          liftIO $ withPushParams env pushOptions name $ \pushParams -> do
            narinfoResponse <- liftIO $ narinfoExists pushParams (toS storeHash)
            let storePathText = NarInfo.storePath narInfo
                store = pushParamsStore pushParams
            storePath <- liftIO $ parseStorePath store (toS storePathText)
            let strategy = pushParamsStrategy pushParams storePath

            case narinfoResponse of
              Right NoContent -> onAlreadyPresent strategy
              Left err
                | isErr err status404 -> runResourceT $ do
                    pathInfo <- newPathInfoFromNarInfo narInfo
                    let fileSize = fromInteger $ NarInfo.fileSize narInfo

                    case readMaybe (NarInfo.compression narInfo) of
                      Nothing -> putErrText $ "Unsupported compression method: " <> NarInfo.compression narInfo
                      Just compressionMethod -> do
                        liftIO $ onAttempt strategy defaultRetryStatus fileSize

                        narStream <- getObject awsEnv $ Amazonka.S3.ObjectKey $ NarInfo.url narInfo

                        res <-
                          runConduit $
                            narStream
                              .| streamCopy pushParams storePath fileSize defaultRetryStatus compressionMethod

                        case res of
                          Left uploadErr ->
                            liftIO $ onError strategy uploadErr
                          Right (uploadResult, uploadNarDetails) -> do
                            -- TODO: Check that the file size matches?
                            -- Copy over details about the NAR from the narinfo.
                            let newNarDetails = uploadNarDetails {undNarSize = NarInfo.narSize narInfo, undNarHash = NarInfo.narHash narInfo}

                            nic <- newNarInfoCreate pushParams storePath pathInfo newNarDetails
                            completeNarUpload pushParams uploadResult nic

                            liftIO $ onDone strategy
                | otherwise -> putErrText $ show err

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

watchStore :: Env -> PushOptions -> Text -> IO ()
watchStore env opts name = do
  withPushParams env opts name $ \pushParams ->
    WatchStore.startWorkers (pushParamsStore pushParams) (numJobs opts) pushParams

-- | Run a command and upload any new paths to the binary cache.
--
-- Registers a post-build hook if the user is trusted.
-- Otherwise, falls back to watching the entire Nix store.
watchExec :: Env -> PushOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExec env pushOptions cacheName cmd args = do
  nixEnv <- getNixEnv

  if InstallationMode.isTrusted nixEnv
    then watchExecDaemon env pushOptions cacheName cmd args
    else do
      putErrText fallbackWarning
      watchExecStore env pushOptions cacheName cmd args
  where
    fallbackWarning =
      color Yellow "WARNING: " <> "failed to register a post-build hook for this command because you're not a trusted user. Falling back to watching the entire Nix store for new paths."

-- | Run a command and push any new paths to the binary cache.
--
-- Requires the user to be a trusted user in a multi-user installation.
watchExecDaemon :: Env -> PushOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExecDaemon env pushOpts cacheName cmd args =
  Daemon.PostBuildHook.withSetup Nothing $ \hookEnv ->
    withTempFile (Daemon.PostBuildHook.tempDir hookEnv) "daemon-log-capture" $ \_ logHandle -> do
      let daemonOptions = DaemonOptions {daemonSocketPath = Just (Daemon.PostBuildHook.daemonSock hookEnv)}
      daemon <- Daemon.new env daemonOptions (Just logHandle) pushOpts cacheName

      exitCode <-
        bracket (startDaemonThread daemon) (shutdownDaemonThread daemon logHandle) $ \_ -> do
          processEnv <- getEnvironment
          let newProcessEnv = Daemon.PostBuildHook.modifyEnv (Daemon.PostBuildHook.envVar hookEnv) processEnv
          let process =
                (System.Process.proc (toS cmd) (toS <$> args))
                  { System.Process.std_out = System.Process.Inherit,
                    System.Process.env = Just newProcessEnv,
                    System.Process.delegate_ctlc = True
                  }
          System.Process.withCreateProcess process $ \_ _ _ processHandle ->
            System.Process.waitForProcess processHandle

      exitWith exitCode
  where
    -- Launch the daemon in the background and subscribe to all push events
    startDaemonThread daemon = do
      daemonThread <- Async.async $ Daemon.run daemon
      daemonChan <- Daemon.subscribe daemon
      return (daemonThread, daemonChan)

    shutdownDaemonThread daemon logHandle (daemonThread, daemonChan) = do
      -- TODO: process and fold events into a state during command execution
      daemonRes <- Async.withAsync (postWatchExec daemonChan) $ \_ -> do
        Daemon.stopIO daemon
        Async.wait daemonThread

      -- Print the daemon log in case there was an internal error
      case toExitCode daemonRes of
        ExitFailure _ -> printLog logHandle
        ExitSuccess -> return ()

    postWatchExec chan = do
      statsRef <- newIORef HashMap.empty
      runConduit $
        C.sourceTMChan chan
          .| C.mapM_ (displayPushEvent statsRef)

    -- Deduplicate events by path and retry status
    displayPushEvent statsRef PushEvent {eventMessage} = liftIO $ do
      stats <- readIORef statsRef
      case eventMessage of
        PushStorePathAttempt path pathSize retryStatus -> do
          case HashMap.lookup path stats of
            Just (progress, prevRetryStatus)
              | prevRetryStatus == retryStatus -> return ()
              | otherwise -> do
                  newProgress <- Daemon.Progress.update progress retryStatus
                  writeIORef statsRef $ HashMap.insert path (newProgress, retryStatus) stats
            Nothing -> do
              progress <- Daemon.Progress.new stderr (toS path) pathSize retryStatus
              writeIORef statsRef $ HashMap.insert path (progress, retryStatus) stats
        PushStorePathProgress path _ newBytes -> do
          case HashMap.lookup path stats of
            Nothing -> return ()
            Just (progress, _) -> Daemon.Progress.tick progress newBytes
        PushStorePathDone path -> do
          mapM_ (Daemon.Progress.complete . fst) (HashMap.lookup path stats)
        PushStorePathFailed path _ -> do
          mapM_ (Daemon.Progress.fail . fst) (HashMap.lookup path stats)
        _ -> return ()

    printLog h = getLineLoop
      where
        getLineLoop = do
          eline <- try $ hGetLine h
          case eline of
            Left e
              | isEOFError e -> return ()
              | otherwise -> putErrText $ "Error reading daemon log: " <> show e
            Right line -> do
              hPutStr stderr line
              getLineLoop

-- | Runs a command while watching the entire Nix store and pushing any new paths.
--
-- Prefer to use the more granular 'watchExecDaemon' whenever possible.
watchExecStore :: Env -> PushOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExecStore env pushOpts name cmd args =
  withPushParams env pushOpts name $ \pushParams -> do
    stdoutOriginal <- hDuplicate stdout
    let process =
          (System.Process.proc (toS cmd) (toS <$> args))
            { System.Process.std_out = System.Process.UseHandle stdoutOriginal
            }
        watch = do
          hDuplicateTo stderr stdout -- redirect all stdout to stderr
          WatchStore.startWorkers (pushParamsStore pushParams) (numJobs pushOpts) pushParams

    (_, exitCode) <-
      Async.concurrently watch $ do
        exitCode <-
          bracketOnError
            (getProcessHandle <$> System.Process.createProcess process)
            ( \processHandle -> do
                -- Terminate the process
                uninterruptibleMask_ (System.Process.terminateProcess processHandle)
                -- Wait for the process to clean up and exit
                _ <- System.Process.waitForProcess processHandle
                -- Stop watching the store and wait for all paths to be pushed
                Signals.raiseSignal Signals.sigINT
            )
            System.Process.waitForProcess

        -- Stop watching the store and wait for all paths to be pushed
        Signals.raiseSignal Signals.sigINT
        return exitCode

    exitWith exitCode
  where
    getProcessHandle (_, _, _, processHandle) = processHandle
