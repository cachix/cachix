{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Cachix.Client.Commands
  ( authtoken
  , create
  , push
  , use
  ) where

import           Crypto.Sign.Ed25519
import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.QSem       as QSem
import           Control.Concurrent.Async     (mapConcurrently)
import           Control.Monad                (forever)
import           Control.Exception.Safe       (MonadThrow, throwM)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString.Base64        as B64
import           Data.Conduit
import           Data.Conduit.Process
import qualified Data.Conduit.List             as CL
import           Data.Conduit.Lzma              ( compress )
import           Data.IORef
import           Data.List                      ( isSuffixOf )
import           Data.Maybe                     ( fromJust )
import           Data.String.Here
import qualified Data.Text                      as T
import           Network.HTTP.Types (status404)
import           Protolude
import           Servant.API
import           Servant.Client
import           Servant.Auth                   ()
import           Servant.Auth.Client
import           Servant.Streaming.Client       ()
import           Servant.API.Generic
import           Servant.Client.Generic
import           System.Directory               ( doesFileExist )
import           System.FSNotify
import           System.IO                      ( stdin, hIsTerminalDevice )
import           System.Process                 ( readProcess )
import           System.Environment             ( lookupEnv )
import qualified Streaming.Prelude             as S

import qualified Cachix.Api                    as Api
import           Cachix.Api.Signing             (fingerprint, passthroughSizeSink, passthroughHashSink)
import qualified Cachix.Types.NarInfoCreate    as Api
import           Cachix.Client.Config           ( Config(..)
                                                , BinaryCacheConfig(..)
                                                , writeConfig
                                                , mkConfig
                                                )
import qualified Cachix.Client.Config          as Config
import           Cachix.Client.Env              ( Env(..) )
import           Cachix.Client.OptionsParser    ( CachixOptions(..) )
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
    Just config -> config { authToken = token }
    Nothing -> mkConfig token
  putStrLn ([hereLit|
Continue by creating a binary cache with:

    $ cachix create <name>

and share it with others over https://<name>.cachix.org
  |] :: Text)

create :: Env -> Text -> IO ()
create Env { config = Nothing } _ = throwIO $ NoConfig "start with: $ cachix authtoken <token>"
create env@Env { config = Just config } name = do
  (PublicKey pk, SecretKey sk) <- createKeypair

  let bc = Api.BinaryCacheCreate $ toS $ B64.encode pk
  res <- (`runClientM` clientenv env) $ Api.create (cachixBCClient name) (Token (toS (authToken config))) bc
  case res of
    -- TODO: handle all kinds of errors
    Left err -> panic $ show err
    Right _ -> do
      -- write signing key to config
      let signingKey = toS $ B64.encode sk
          bcc = BinaryCacheConfig name signingKey

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

      |] :: Text)

use :: Env -> Text -> Bool -> IO ()
use env name shouldEchoNixOS = do
  -- 1. get cache public key
  res <- (`runClientM` clientenv env) $ Api.get (cachixBCClient name)
  case res of
    -- TODO: handle 404
    Left err -> panic $ show err
    Right binaryCache -> do
      eitherNixVersion <- getNixVersion
      case eitherNixVersion of
        Left err -> panic err
        Right nixVersion -> do
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
          addBinaryCache binaryCache $
            if shouldEchoNixOS
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
  mapConcurrentlyBounded 4 (pushStorePath env name) paths
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
pushStorePath env name storePath = do
  -- use secret key from config or env
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
    (Api.NarInfoC storeHash)
  case res of
    Right NoContent -> return ()
    Left err | isErr err status404 -> go sk
             | otherwise -> panic $ show err -- TODO: retry
    where
      go sk = do
        let (storeHash, storeSuffix) = splitStorePath $ toS storePath
        putStrLn $ "pushing " <> storePath

        narSizeRef <- liftIO $ newIORef 0
        fileSizeRef <- liftIO $ newIORef 0
        narHashRef <- liftIO $ newIORef ("" :: Text)
        fileHashRef <- liftIO $ newIORef ("" :: Text)

        -- stream store path as xz compressed nar file
        let cmd = "nix-store --dump " <> toS storePath
        (ClosedStream, (stdoutStream, closeStdout), ClosedStream, cph) <- streamingProcess $ shell cmd
        let stream'
              = stdoutStream
             .| passthroughSizeSink narSizeRef
             .| passthroughHashSink narHashRef
             .| compress Nothing
             .| passthroughSizeSink fileSizeRef
             .| passthroughHashSink fileHashRef

            conduitToStreaming :: S.Stream (S.Of ByteString) (ResourceT IO) ()
            conduitToStreaming = runConduit (transPipe lift stream' .| CL.mapM_ S.yield)
        -- for now we need to use letsencrypt domain instead of cloudflare due to its upload limits
        let newClientEnv = (clientenv env) {
              baseUrl = (baseUrl (clientenv env)) { baseUrlHost = toS name <> "." <> baseUrlHost (baseUrl (clientenv env))}
            }
        -- TODO: http retry: retry package?
        NoContent <- escalate <=< (`runClientM` newClientEnv) $ Api.createNar
          (cachixBCClient name)
          (contentType (Proxy :: Proxy Api.XNixNar), conduitToStreaming)
        closeStdout
        exitcode <- waitForStreamingProcess cph
        -- TODO: print process stderr?
        when (exitcode /= ExitSuccess) $
          panic $ "Failed with " <> show exitcode <> ": " <> toS cmd

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
        NoContent <- escalate <=< (`runClientM` clientenv env) $ Api.createNarinfo
          (cachixBCClient name)
          (Api.NarInfoC storeHash)
          nic
        return ()


-- Utils

-- TODO: should one error abort the whole pushing process? Or do a retry? Or keep going and then fail?
escalate :: (Exception exc, MonadThrow m) => Either exc a -> m a
escalate = escalateAs identity

escalateAs :: (Exception exc, MonadThrow m) => (l -> exc) -> Either l a -> m a
escalateAs f = either (throwM . f) pure

mapConcurrentlyBounded :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyBounded bound action items = do
  qs <- QSem.newQSem bound
  let wrapped x = bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) (action x)
  mapConcurrently wrapped items

splitStorePath :: Text -> (Text, Text)
splitStorePath storePath =
  (T.take 32 (T.drop 11 storePath), T.drop 44 storePath)
