{-# LANGUAGE QuasiQuotes #-}
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
import           Control.Concurrent.MSem
import           Control.Concurrent.Async
import           Control.Monad                (forever)
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
import           Servant.Generic
import           System.Directory               ( doesFileExist )
import           System.FSNotify
import           System.IO                      ( stdin, hIsTerminalDevice )
import           System.Process                 ( readProcessWithExitCode, readProcess )
import           System.Environment             ( lookupEnv )
import qualified Streaming.Prelude             as S

import qualified Cachix.Api                    as Api
import           Cachix.Api.Signing             (fingerprint, passthroughSizeSink, passthroughHashSink)
import           Cachix.Client.Config           ( Config(..)
                                                , BinaryCacheConfig(..)
                                                , writeConfig
                                                , mkConfig
                                                )
import qualified Cachix.Client.Config          as Config
import           Cachix.Client.InstallationMode
import           Cachix.Client.NixVersion       ( getNixMode
                                                , NixMode
                                                )
import qualified Cachix.Client.NixConf         as NixConf
import           Cachix.Client.Servant


cachixClient :: Api.CachixAPI AsClient
cachixClient = fromServant $ client Api.servantApi

cachixBCClient :: Text -> Api.BinaryCacheAPI AsClient
cachixBCClient name = fromServant $ Api.cache cachixClient name

authtoken :: ClientEnv -> Maybe Config -> Text -> IO ()
authtoken _ maybeConfig token = do
  -- TODO: check that token actually authenticates!
  writeConfig $ case maybeConfig of
    Just config -> config { authToken = token }
    Nothing -> mkConfig token
  putStrLn ([hereLit|
Continue by creating a binary cache with:

    $ cachix create <name>

and share it with others over https://<name>.cachix.org
  |] :: Text)

create :: ClientEnv -> Maybe Config -> Text -> IO ()
create _ Nothing _ = throwIO $ NoConfig "start with: $ cachix authtoken <token>"
create env (Just config@Config{..}) name = do
  (PublicKey pk, SecretKey sk) <- createKeypair

  let bc = Api.BinaryCacheCreate $ toS $ B64.encode pk
  res <- (`runClientM` env) $ Api.create (cachixBCClient name) (Token (toS authToken)) bc
  case res of
    -- TODO: handle all kinds of errors
    Left err -> panic $ show err
    Right _ -> do
      -- write signing key to config
      let signingKey = toS $ B64.encode sk
          bcc = BinaryCacheConfig name signingKey
      writeConfig $ config { binaryCaches = binaryCaches <> [bcc] }

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

use :: ClientEnv -> Maybe Config -> Text -> IO ()
use env _ name = do
  -- 1. get cache public key
  res <- (`runClientM` env) $ Api.get (cachixBCClient name)
  case res of
    -- TODO: handle 404
    Left err -> panic $ show err
    Right binaryCache -> do
      eitherNixMode <- getNixMode
      case eitherNixMode of
        Left err -> panic err
        Right nixMode -> do
          user <- getUser
          nc <- NixConf.read NixConf.Global
          isTrusted <- isTrustedUser $ NixConf.readLines (catMaybes [nc]) NixConf.isTrustedUsers
          isNixOS <- doesFileExist "/etc/NIXOS"
          let nixEnv = NixEnv
                { nixMode = nixMode
                , isRoot = user == "root"
                , isTrusted = isTrusted
                , isNixOS = isNixOS
                }
          addBinaryCache binaryCache $ getInstallationMode nixEnv


-- TODO: lots of room for perfomance improvements
push :: ClientEnv -> Maybe Config -> Text -> [Text] -> Bool -> IO ()
push env config name rawPaths False = do
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
  (exitcode, out, err) <- readProcessWithExitCode "nix-store" (fmap toS (["-qR"] <> inputStorePaths)) mempty

  -- TODO: make pool size configurable, on beefier machines this could be doubled
  _ <- mapPool 4 (T.lines (toS out)) $ pushStorePath env config name
  putText "All done."
push env config name _ True = withManager $ \mgr -> do
  _ <- watchDir mgr "/nix/store" filterF action
  putText "Watching /nix/store for new builds ..."
  forever $ threadDelay 1000000
  where
    action :: Action
    action (Removed fp _) = pushStorePath env config name $ toS $ dropEnd 5 fp
    action _ = return ()

    filterF :: ActionPredicate
    filterF (Removed fp _) | ".lock" `isSuffixOf` fp = True
    filterF _ = False

    dropEnd :: Int -> [a] -> [a]
    dropEnd index xs = take (length xs - index) xs

pushStorePath :: ClientEnv -> Maybe Config -> Text -> Text -> IO ()
pushStorePath env config name storePath = do
  -- use secret key from config or env
  maybeEnvSK <- lookupEnv "CACHIX_SIGNING_KEY"
  let matches Config{..} = filter (\bc -> Config.name bc == name) binaryCaches
      maybeBCSK = case config of
        Nothing -> Nothing
        Just c -> Config.secretKey <$> head (matches c)
      -- TODO: better error msg
      sk = SecretKey $ toS $ B64.decodeLenient $ toS $ fromJust $ maybeBCSK <|> toS <$> maybeEnvSK <|> panic "You need to: export CACHIX_SIGNING_KEY=XXX"
      (storeHash, _) = splitStorePath $ toS storePath
  -- Check if narinfo already exists
  -- TODO: query also cache.nixos.org? server-side?
  res <- (`runClientM` env) $ Api.narinfoHead
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
        (ClosedStream, (source, close), ClosedStream, cph) <- streamingProcess $ shell ("nix-store --dump " <> toS storePath)
        let stream'
              = source
             .| passthroughSizeSink narSizeRef
             .| passthroughHashSink narHashRef
             .| compress Nothing
             .| passthroughSizeSink fileSizeRef
             .| passthroughHashSink fileHashRef

            conduitToStreaming :: S.Stream (S.Of ByteString) (ResourceT IO) ()
            conduitToStreaming = runConduit (transPipe lift stream' .| CL.mapM_ S.yield)
        -- for now we need to use letsencrypt domain instead of cloudflare due to its upload limits
        let newEnv = env {
              baseUrl = (baseUrl env) { baseUrlHost = toS name <> "." <> baseUrlHost (baseUrl env)}
            }
        -- TODO: http retry: retry package?
        res <- (`runClientM` newEnv) $ Api.createNar
          (cachixBCClient name)
          (contentType (Proxy :: Proxy Api.XNixNar), conduitToStreaming)
        close
        exitcode <- waitForStreamingProcess cph
        case res of
          Left err -> panic $ show err
          Right NoContent -> do
            narSize <- readIORef narSizeRef
            narHashB16 <- readIORef narHashRef
            fileHash <- readIORef fileHashRef
            fileSize <- readIORef fileSizeRef

            -- TODO: #3: implement using pure haskell
            narHash <- ("sha256:" <>) . T.strip . toS <$> readProcess "nix-hash" ["--type", "sha256", "--to-base32", toS narHashB16] mempty

            (exitcode, out, err) <- readProcessWithExitCode "nix-store" ["-q", "--deriver", toS storePath] mempty
            let deriverRaw = T.strip $ toS out
                deriver = if deriverRaw == "unknown-deriver"
                          then deriverRaw
                          else T.drop 11 deriverRaw

            (exitcode, out, err) <- readProcessWithExitCode "nix-store" ["-q", "--references", toS storePath] mempty

            let
                references = sort $ T.lines $ T.strip $ toS out
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
            -- Upload narinfo with signature
            res <- (`runClientM` env) $ Api.createNarinfo
              (cachixBCClient name)
              (Api.NarInfoC storeHash)
              nic
            case res of
              Left err -> panic $ show err -- TODO: handle json errors
              Right NoContent -> return ()


-- Utils


mapPool :: Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
mapPool limit xs f = do
    sem <- new limit
    mapConcurrently (with sem . f) xs

splitStorePath :: Text -> (Text, Text)
splitStorePath storePath =
  (T.take 32 (T.drop 11 storePath), T.drop 44 storePath)
