{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Commands
  ( authtoken,
    generateKeypair,
    push,
    watchStore,
    watchExec,
    watchExecDaemon,
    use,
    remove,
    pin,
  )
where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import Cachix.Client.Commands.Push
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Daemon as Daemon
import qualified Cachix.Client.Daemon.PostBuildHook as Daemon.PostBuildHook
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
import qualified Cachix.Client.WatchStore as WatchStore
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.PinCreate as PinCreate
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import qualified Control.Concurrent.Async as Async
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Crypto.Sign.Ed25519 (PublicKey (PublicKey), createKeypair)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.String (String)
import Data.String.Here
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hercules.CNix.Store (storePathToPath, withStore)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API (NoContent)
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Console.Concurrent
import System.Console.Pretty
import System.Console.Regions
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.IO
  ( BufferMode (..),
    hClose,
    hFlush,
    hGetChar,
    hGetContents,
    hGetLine,
    hIsEOF,
    hIsTerminalDevice,
    hPutChar,
    hSetBuffering,
    hSetEncoding,
    hSetNewlineMode,
    noNewlineTranslation,
    universalNewlineMode,
    utf8,
  )
import System.IO.Temp (withSystemTempFile)
import qualified System.Posix.IO as System.IO
import System.Posix.Pty
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Terminal as System.Terminal
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

watchExec :: Env -> PushOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExec env pushOpts name cmd args = withPushParams env pushOpts name $ \pushParams -> do
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

watchExecDaemon :: Env -> PushOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExecDaemon env pushOpts cacheName cmd args = do
  Daemon.PostBuildHook.withSetup Nothing $ \daemonSock userConfEnv -> do
    withSystemTempFile "daemon-log-capture" $ \logFile logHandle -> do
      let daemonOptions = DaemonOptions {daemonSocketPath = Just daemonSock}
      daemon <- Daemon.new env daemonOptions (Just logHandle) pushOpts cacheName
      daemonThread <- Async.async $ Daemon.run daemon

      let copyHandleToHandle hIn hOut = do
            eeof <- try $ hIsEOF hIn
            case eeof of
              Left (e :: IOException) -> do
                putStrLn ("IO Exception" :: Text)
                -- print e
                return ()
              Right eof ->
                unless eof $ do
                  -- return ()
                  -- hGetChar hIn >>= outputConcurrent . T.singleton
                  -- hGetLine hIn >>= hPutStr hOut
                  buffer <- BS.hGetSome hIn 4096
                  BS.hPut hOut buffer
                  -- hGetChar hIn >>= hPutChar hOut
                  -- update
                  copyHandleToHandle hIn hOut

      processEnv <- getEnvironment
      (readFd, writeFd) <- System.Terminal.openPseudoTerminal
      (read2Fd, write2Fd) <- System.Terminal.openPseudoTerminal
      anotherHandle <- System.IO.fdToHandle write2Fd
      -- hSetBuffering writeHandle NoBuffering
      -- hSetEncoding readHandle utf8
      -- hSetEncoding writeHandle utf8
      -- hSetNewlineMode readHandle universalNewlineMode
      let makeRaw :: TerminalAttributes -> TerminalAttributes
          makeRaw attrs =
            foldl
              withoutMode
              attrs
              [ IgnoreBreak, -- IGNBRK
                InterruptOnBreak, -- BRKINT
                MarkParityErrors, -- PARMRK
                StripHighBit, -- ISTRIP
                MapLFtoCR, -- INLCR
                IgnoreCR, -- IGNCR
                MapCRtoLF, -- ICRNL
                StartStopOutput, -- IXON
                ProcessOutput, -- OPOST
                EnableEcho, -- ECHO
                EchoLF, -- ECHONL
                ProcessInput, -- ICANON
                KeyboardInterrupts, -- ISIG
                ExtendedFunctions, -- IEXTEN
                EnableParity -- PARENB
              ]
      attrs <- System.Terminal.getTerminalAttributes System.IO.stdOutput
      let newModes = makeRaw attrs
      System.Terminal.setTerminalAttributes readFd newModes Immediately
      System.Terminal.setTerminalAttributes writeFd newModes Immediately
      -- System.Terminal.setTerminalAttributes System.IO.stdOutput newModes Immediately

      readHandle <- System.IO.fdToHandle readFd
      writeHandle <- System.IO.fdToHandle writeFd
      hSetBuffering readHandle NoBuffering
      hSetBuffering stdout LineBuffering
      hSetNewlineMode readHandle noNewlineTranslation
      hSetNewlineMode writeHandle noNewlineTranslation
      hSetEncoding readHandle utf8
      hSetEncoding writeHandle utf8

      -- System.IO.dupTo System.IO.stdOutput writeFd
      -- System.IO.dupTo System.IO.stdError writeFd

      -- flip System.IO.dupTo readFd =<< (System.IO.handleToFd stdout)
      -- hSetBuffering ptmxHandle NoBuffering
      let process =
            (System.Process.proc (toS cmd) (toS <$> args))
              { System.Process.std_out = System.Process.UseHandle anotherHandle, -- System.Process.UseHandle writeHandle,
                System.Process.std_err = System.Process.UseHandle writeHandle,
                System.Process.std_in = System.Process.Inherit,
                System.Process.env = Just (processEnv ++ [("NIX_USER_CONF_FILES", toS userConfEnv)]),
                System.Process.delegate_ctlc = True
              }

      -- (_, _, h, processHandle) <- System.Process.createProcess process
      exitCode <- System.Process.withCreateProcess process $ \_ h _ processHandle -> do
        Async.withAsync (copyHandleToHandle readHandle stdout) $ \logThread -> do
          exitCode <- System.Process.waitForProcess processHandle
          hFlush readHandle
          -- Async.wait logThread
          threadDelay (100 * 1000)
          return exitCode

      region <- openConsoleRegion Linear

      let getStats isDone = do
            stats <- Daemon.renderStats daemon
            pure $
              mconcat
                [ if isDone then "\nPushed store paths to Cachix.\n" else "\nPushing store paths...\n",
                  stats
                ]

      let update isDone = do
            setConsoleRegion region =<< getStats isDone

      let loopUpdate = do
            update False
            threadDelay (100 * 1000)
            loopUpdate

      -- Stop the daemon and wait for all paths to be pushed
      Async.withAsync loopUpdate $ \_ -> do
        Daemon.stopIO daemon
        daemonResult <- Async.wait daemonThread
        threadDelay (100 * 1000)
        return ()

      -- when (isLeft daemonResult) $ do
      --   -- On error, print log?
      hClose logHandle
      -- Output the contents of the daemon log to stderr
      -- withFile logFile ReadMode $ hGetContents >=> putStr

      finishConsoleRegion region =<< getStats True

      exitWith exitCode
