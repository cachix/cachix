{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Cachix.Client.Commands
  ( authtoken
  , create
  , generateKeypair
  , push
  , use
  ) where

import           Crypto.Sign.Ed25519
import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.QSem       as QSem
import           Control.Concurrent.Async     (mapConcurrently)
import           Control.Monad                (forever, (>=>))
import           Control.Exception.Safe       (MonadThrow, throwM)
import           Control.Retry                (recoverAll, RetryStatus(rsIterNumber))
import qualified Data.ByteString.Base64        as B64
import           Data.Conduit
import           Data.Default                  (def)
import           Data.Conduit.Process
import           Data.Conduit.Lzma              ( compress )
import           Data.IORef
import           Data.List                      ( isSuffixOf )
import           Data.Maybe                     ( fromJust )
import           Data.String.Here
import qualified Data.Text                      as T
import           Network.HTTP.Types             (status404, status401)
import           Protolude
import           Servant.API
import           Servant.Auth                   ()
import           Servant.Auth.Client
import           Servant.API.Generic
import           Servant.Client.Generic
import           Servant.Client.Streaming
import           Servant.Conduit                ()
import           System.Directory               ( doesFileExist )
import           System.FSNotify
import           System.IO                      ( stdin, hIsTerminalDevice )
import           System.Process                 ( readProcess )
import           System.Environment             ( lookupEnv )

import qualified Cachix.Api                    as Api
import           Cachix.Api.Error
import           Cachix.Api.Signing             (fingerprint, passthroughSizeSink, passthroughHashSink)
import qualified Cachix.Types.NarInfoCreate    as Api
import qualified Cachix.Types.BinaryCacheCreate as Api
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import           Cachix.Client.Config           ( Config(..)
                                                , BinaryCacheConfig(..)
                                                , writeConfig
                                                , mkConfig
                                                )
import qualified Cachix.Client.Config          as Config
import           Cachix.Client.Env              ( Env(..) )
import           Cachix.Client.Exception        ( CachixException(..) )
import           Cachix.Client.OptionsParser    ( CachixOptions(..), UseOptions(..) )
import           Cachix.Client.InstallationMode
import           Cachix.Client.NixVersion       ( getNixVersion )
import qualified Cachix.Client.NixConf         as NixConf
import           Cachix.Client.Servant


cachixClient :: Api.CachixAPI (AsClientT ClientM)
cachixClient = fromServant $ client Api.servantApi

cachixBCClient :: Text -> Api.BinaryCacheAPI (AsClientT ClientM)
cachixBCClient name = fromServant $ Api.cache cachixClient name

authtoken :: Env -> Text -> IO ()
authtoken env token = do
  -- TODO: check that token actually authenticates!
  writeConfig (configPath (cachixoptions env)) $ case config env of
    Just config -> config { authToken = Token (toS token) }
    Nothing -> mkConfig token
  putStrLn ([hereLit|
Continue by creating a binary cache at https://cachix.org
  |] :: Text)

create :: Env -> Text -> IO ()
create _ _ =
  throwIO $ DeprecatedCommand "Create command has been deprecated. Please visit https://cachix.org to create a binary cache."

generateKeypair :: Env -> Text -> IO ()
generateKeypair Env { config = Nothing } _ = throwIO $ NoConfig "Start with visiting https://cachix.org and copying the token to $ cachix authtoken <token>"
generateKeypair env@Env { config = Just config } name = do
  (PublicKey pk, SecretKey sk) <- createKeypair

  let signingKey = toS $ B64.encode sk
      signingKeyCreate = SigningKeyCreate.SigningKeyCreate (toS $ B64.encode pk)
      bcc = BinaryCacheConfig name signingKey

  -- we first validate if key can be added to the binary cache
  escalate =<< ((`runClientM` clientenv env) $ Api.createKey (cachixBCClient name) (authToken config) signingKeyCreate)

  -- if key was successfully added, write it to the config
  -- TODO: this breaks if more than one key is added, see #27
  writeConfig (configPath (cachixoptions env)) $
    config { binaryCaches = binaryCaches config <> [bcc] }

  putStrLn ([iTrim|
Signing key has been saved on your local machine. To populate
your binary cache:

    $ nix-build | cachix push ${name}

Or if you'd like to use the signing key on another machine or CI:

    $ export CACHIX_SIGNING_KEY=${signingKey}
    $ nix-build | cachix push ${name}

To instruct Nix to use the binary cache:

    $ cachix use ${name}

IMPORTANT: Make sure to make a backup for the signing key above, as you have the only copy.
  |] :: Text)

envToToken :: Env -> Token
envToToken env =
  maybe (Token "") authToken (config env)

notAuthenticatedBinaryCache :: CachixException
notAuthenticatedBinaryCache =
  AccessDeniedBinaryCache "You must first authenticate using:  $ cachix authtoken <token>"

accessDeniedBinaryCache :: Text -> CachixException
accessDeniedBinaryCache name =
  AccessDeniedBinaryCache $ "You don't seem to have API access to binary cache " <> name

use :: Env -> Text -> UseOptions -> IO ()
use env name useOptions = do
  -- 1. get cache public key
  res <- (`runClientM` clientenv env) $ Api.get (cachixBCClient name) (envToToken env)
  case res of
    Left err | isErr err status401 && isJust (config env) -> throwM $ accessDeniedBinaryCache name
             | isErr err status401 -> throwM notAuthenticatedBinaryCache
             | isErr err status404 -> throwM $ BinaryCacheNotFound $ "Binary cache" <> name <> " does not exist."
             | otherwise -> throwM err
    Right binaryCache -> do
      nixVersion <- escalateAs UnsupportedNixVersion =<< getNixVersion
      user <- getUser
      nc <- NixConf.read NixConf.Global
      isTrusted <- isTrustedUser $ NixConf.readLines (catMaybes [nc]) NixConf.isTrustedUsers
      isNixOS <- doesFileExist "/etc/NIXOS"
      let nixEnv = NixEnv
            { nixVersion = nixVersion
            , isRoot = user == "root"
            , isTrusted = isTrusted
            , isNixOS = isNixOS
            }
      addBinaryCache (config env) binaryCache useOptions $
        if useNixOS useOptions
        then EchoNixOS nixVersion
        else getInstallationMode nixEnv


-- TODO: lots of room for perfomance improvements
push :: Env -> Text -> [Text] -> Bool -> IO ()
push env name rawPaths False = do
  hasNoStdin <- hIsTerminalDevice stdin
  when (not hasNoStdin && not (null rawPaths)) $ throwIO $ AmbiguousInput "You provided both stdin and store path arguments, pick only one to proceed."
  inputStorePaths <-
    if hasNoStdin
    then return rawPaths
    else T.words <$> getContents

  -- TODO: if empty, take whole nix store and warn: nix store-path --all
  when (null inputStorePaths) $ throwIO $ NoInput "You need to specify store paths either as stdin or as a cli argument"

  -- Query list of paths
  -- TODO: split args if too big
  paths <- T.lines . toS <$> readProcess "nix-store" (fmap toS (["-qR"] <> inputStorePaths)) mempty

  -- TODO: make pool size configurable, on beefier machines this could be doubled
  _ <- mapConcurrentlyBounded 4 (pushStorePath env name) paths
  putText "All done."
push env name _ True = withManager $ \mgr -> do
  _ <- watchDir mgr "/nix/store" filterF action
  putText "Watching /nix/store for new builds ..."
  forever $ threadDelay 1000000
  where
    action :: Action
#if MIN_VERSION_fsnotify(0,3,0)
    action (Removed fp _ _) =
#else
    action (Removed fp _) =
#endif
      pushStorePath env name $ toS $ dropEnd 5 fp
    action _ = return ()

    filterF :: ActionPredicate
#if MIN_VERSION_fsnotify(0,3,0)
    filterF (Removed fp _ _)
#else
    filterF (Removed fp _)
#endif
      | ".lock" `isSuffixOf` fp = True
    filterF _ = False

    dropEnd :: Int -> [a] -> [a]
    dropEnd index xs = take (length xs - index) xs

pushStorePath :: Env -> Text -> Text -> IO ()
pushStorePath env name storePath = retryPath $ \retrystatus -> do
  -- use secret key from config or env
  -- TODO: this shouldn't happen for each store path
  maybeEnvSK <- lookupEnv "CACHIX_SIGNING_KEY"
  let matches Config{..} = filter (\bc -> Config.name bc == name) binaryCaches
      maybeBCSK = case config env of
        Nothing -> Nothing
        Just c -> Config.secretKey <$> head (matches c)
      -- TODO: better error msg
      sk = SecretKey $ toS $ B64.decodeLenient $ toS $ fromJust $ maybeBCSK <|> toS <$> maybeEnvSK <|> panic "You need to: export CACHIX_SIGNING_KEY=XXX"
      (storeHash, _) = splitStorePath $ toS storePath
  -- Check if narinfo already exists
  -- TODO: query also cache.nixos.org? server-side?
  res <- (`runClientM` clientenv env) $ Api.narinfoHead
    (cachixBCClient name)
    (envToToken env)
    (Api.NarInfoC storeHash)
  case res of
    Right NoContent -> pass -- we're done as store path is already in the cache
    Left err | isErr err status404 -> go sk retrystatus
             | isErr err status401 && isJust (config env) -> throwM $ accessDeniedBinaryCache name
             | isErr err status401 -> throwM notAuthenticatedBinaryCache
             | otherwise -> throwM err
    where
      retryText :: RetryStatus -> Text
      retryText retrystatus =
        if rsIterNumber retrystatus == 0
        then ""
        else "(retry #" <> show (rsIterNumber retrystatus) <> ") "
      go sk retrystatus = do
        let (storeHash, storeSuffix) = splitStorePath $ toS storePath
        -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
        putStr $ "pushing " <> retryText retrystatus <> storePath <> "\n"

        narSizeRef <- liftIO $ newIORef 0
        fileSizeRef <- liftIO $ newIORef 0
        narHashRef <- liftIO $ newIORef ("" :: Text)
        fileHashRef <- liftIO $ newIORef ("" :: Text)

        -- stream store path as xz compressed nar file
        let cmd = proc "nix-store" ["--dump", toS storePath]
        (ClosedStream, (stdoutStream, closeStdout), ClosedStream, cph) <- streamingProcess cmd
        let stream'
              = stdoutStream
             .| passthroughSizeSink narSizeRef
             .| passthroughHashSink narHashRef
             .| compress Nothing
             .| passthroughSizeSink fileSizeRef
             .| passthroughHashSink fileHashRef

        -- for now we need to use letsencrypt domain instead of cloudflare due to its upload limits
        let newClientEnv = (clientenv env) {
              baseUrl = (baseUrl (clientenv env)) { baseUrlHost = toS name <> "." <> baseUrlHost (baseUrl (clientenv env))}
            }
        void $ (`withClientM` newClientEnv)
            (Api.createNar (cachixBCClient name) stream')
            $ escalate >=> \NoContent -> do
                closeStdout
                exitcode <- waitForStreamingProcess cph
                -- TODO: print process stderr?
                when (exitcode /= ExitSuccess) $ throwM $ NarStreamingError exitcode $ show cmd

                narSize <- readIORef narSizeRef
                narHashB16 <- readIORef narHashRef
                fileHash <- readIORef fileHashRef
                fileSize <- readIORef fileSizeRef

                -- TODO: #3: implement using pure haskell
                narHash <- ("sha256:" <>) . T.strip . toS <$> readProcess "nix-hash" ["--type", "sha256", "--to-base32", toS narHashB16] mempty

                deriverRaw <- T.strip . toS <$> readProcess "nix-store" ["-q", "--deriver", toS storePath] mempty
                let deriver = if deriverRaw == "unknown-deriver"
                              then deriverRaw
                              else T.drop 11 deriverRaw

                references <- sort . T.lines . T.strip . toS <$> readProcess "nix-store" ["-q", "--references", toS storePath] mempty

                let
                    fp = fingerprint storePath narHash narSize references
                    sig = dsign sk fp
                    nic = Api.NarInfoCreate
                      { cStoreHash = storeHash
                      , cStoreSuffix = storeSuffix
                      , cNarHash = narHash
                      , cNarSize = narSize
                      , cFileSize = fileSize
                      , cFileHash = fileHash
                      , cReferences = fmap (T.drop 11) references
                      , cDeriver = deriver
                      , cSig = toS $ B64.encode $ unSignature sig
                      }

                escalate $ Api.isNarInfoCreateValid nic

                -- Upload narinfo with signature
                escalate <=< (`runClientM` clientenv env) $ Api.createNarinfo
                  (cachixBCClient name)
                  (Api.NarInfoC storeHash)
                  nic


-- Utils

-- Retry up to 5 times for each store path.
-- Catches all exceptions except skipAsyncExceptions
retryPath :: (RetryStatus -> IO a) -> IO a
retryPath = recoverAll def

mapConcurrentlyBounded :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyBounded bound action items = do
  qs <- QSem.newQSem bound
  let wrapped x = bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) (action x)
  mapConcurrently wrapped items

splitStorePath :: Text -> (Text, Text)
splitStorePath storePath =
  (T.take 32 (T.drop 11 storePath), T.drop 44 storePath)
