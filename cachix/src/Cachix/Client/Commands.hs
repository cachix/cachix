{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Cachix.Client.Commands
  ( authtoken
  , create
  , sync
  , use
  ) where

import           Crypto.Sign.Ed25519
import           Crypto.Hash
import           Control.Concurrent.MSem
import           Control.Concurrent.Async
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString               as BS
import qualified Data.ByteArray                as BA
-- TODO: use cryptonite encoding instead
-- import qualified Data.ByteArray.Encoding       as BAE
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Base16        as B16
import           Data.Conduit
import           Data.Conduit.Process
import qualified Data.Conduit.Combinators      as CC
import qualified Data.Conduit.List             as CL
import           Data.Conduit.Lzma              ( compress )
import           Data.IORef

import           Data.String.Here
import qualified Data.Text                      as T
import           Protolude
import           Servant.API
import           Servant.Client
import           Servant.Auth
import           Servant.Auth.Client
import           Servant.Streaming.Client
import           Servant.Generic
import           System.Process                 ( readProcessWithExitCode, readProcess )
import           System.Environment             ( lookupEnv )
import qualified Streaming.Prelude        as S
import           Web.Cookie                     ( SetCookie )


import qualified Cachix.Api                    as Api
import           Cachix.Client.Config           ( Config(..)
                                                , BinaryCacheConfig(..)
                                                , writeConfig
                                                , mkConfig
                                                )
import           Cachix.Client.NixVersion       ( getNixMode
                                                , NixMode(..)
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
  |] :: Text)

create :: ClientEnv -> Maybe Config -> Text -> IO ()
create _ Nothing _ = panic "well, you need to authtoken first."
create env (Just config@Config{..}) name = do
  (PublicKey pk, SecretKey sk) <- createKeypair

  let bc = Api.BinaryCacheCreate $ toS $ B64.encode pk
  res <- (`runClientM` env) $ Api.create (cachixBCClient name) (Token (toS authToken)) bc
  case res of
    -- TODO: handle all kinds of errors
    Left err -> putStrLn $ "API Error: " ++ show err
    Right _ -> do
      -- write signing key to config
      let bcc = BinaryCacheConfig name $ toS $ B64.encode sk
      writeConfig $ config { binaryCaches = binaryCaches <> [bcc] }

      putStrLn ([hereLit|
      -- TODO: tell how to use signing key and public cache
      |] :: Text)

use :: ClientEnv -> Maybe Config -> Text -> IO ()
use env _ name = do
  -- 1. get cache public key
  res <- (`runClientM` env) $ Api.get (cachixBCClient name)
  case res of
    -- TODO: handle 404
    Left err -> putStrLn $ "API Error: " ++ show err
    Right Api.BinaryCache{..} -> do
      -- 2. Detect Nix version
      nixMode <- getNixMode
      case nixMode of
        Left err -> panic err
        Right Nix1XX -> panic "Please upgrade to Nix 2.0.1"
         -- TODO: require sudo, use old naming
        Right Nix20 -> panic "Please upgrade to Nix 2.0.1"
         -- TODO: If Nix 2.0, skip trusted user check, require sudo
         -- TODO: NixOS, abort :(
         {-
         maybeUser <- lookupEnv "USER"
         if isRoot (toS <$> maybeUser)
         then return ()
         else return ()
         -}
        Right Nix201 -> do
           -- 2. Detect if is a trusted user (or running single mode?)
           nc <- NixConf.read NixConf.Global
           if isTrustedUser nc
           -- If is a trusted user, populate user nix.conf
           then NixConf.update NixConf.Local $ addSubstituters name publicSigningKeys
           -- if not, tell user what to do (TODO: require sudo to do it)
           else panic "TODO"

fqdn :: Text -> Text
fqdn name = name <> ".cachix.org"

addSubstituters :: Text -> [Text] -> Maybe NixConf.NixConf -> NixConf.NixConf
addSubstituters name pks (Just nixconf) = undefined -- TODO
addSubstituters name pks Nothing =
  NixConf.NixConf
    [ NixConf.Substituters ["https://" <> fqdn name]
    , NixConf.TrustedPublicKeys (fmap (\pk-> fqdn name <> "-1:" <> pk) pks)
    ]

-- TODO: well, define this one
isTrustedUser x = True

isRoot :: Maybe Text -> Bool
isRoot Nothing = panic "$USER is not set"
isRoot (Just user) = user == "root"

mapPool :: Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
mapPool max xs f = do
    sem <- new max
    mapConcurrently (with sem . f) xs

-- TODO: lots of room for perfomance improvements
sync :: ClientEnv -> Maybe Config -> Maybe Text -> IO ()
sync env _ (Just name) = do
  -- Read paths from stdin
  --TODO: inputStorePaths <- T.lines <$> getContents
  let inputStorePaths = ["/nix/store/0011l8r9gq9vnl8jmn78m9b8p0gz6c0m-hourglass-0.2.10"]

  -- TODO:
  let sk = SecretKey "TODO"

  -- Query list of paths
  (exitcode, out, err) <- readProcessWithExitCode "nix-store" (["-qR"] <> inputStorePaths) mempty

  let storePaths :: [Text]
      storePaths = T.lines $ toS out
      numStorePaths = show (length storePaths) :: Text
  putStrLn $ "Asking upstream how many of " <> numStorePaths <> " are already cached."
  -- TODO: mass query cachix if narinfo already exist
  -- TODO: query also cache.nixos.org? server-side?
  -- TODO: print how many will be uploaded

  -- TODO: make pool size configurable, on beefier machines this could be doubled
  successes <- mapPool 4 storePaths $ \storePath -> do
    putStrLn $ "*** Syncing " <> storePath

    narSizeRef <- liftIO $ newIORef 0
    narHashRef <- liftIO $ newIORef ("" :: Text)

    -- stream store path as xz compressed nar file
    (ClosedStream, (source, close), ClosedStream, cph) <- streamingProcess $ shell ("nix-store --dump " <> toS storePath)
    let sizeSink = CC.foldM (\p -> \n -> return (p + BS.length n)) 0

        hashSink :: MonadIO m => Consumer ByteString m (Context SHA256)
        hashSink = CC.foldM (\p -> \n -> return (hashUpdate p n)) hashInit

        stream' = source
               .| passthroughSink (sizeSink) (liftIO . writeIORef narSizeRef)
               .| passthroughSink (hashSink) (liftIO . writeIORef narHashRef . toS . B16.encode . BS.pack . BA.unpack . hashFinalize)
               .| compress Nothing

        streamHoist :: S.Stream (S.Of ByteString) (ResourceT IO) ()
        streamHoist = hoist lift stream' $$ CL.mapM_ S.yield
    -- TODO: http retry
    res <- (`runClientM` env) $ Api.createNar
      (cachixBCClient name)
      (Api.NarC (T.take 32 (T.drop 11 (toS storePath))))
      (contentType (Proxy :: Proxy Api.XNixNar), streamHoist)
    close
    ec <- waitForStreamingProcess cph
    case res of
      Left err -> panic $ show err
      Right NoContent -> do
        narSize <- readIORef narSizeRef
        narHash <- readIORef narHashRef

        -- Well ****, Nix base32 doesn't adhere RFC 4648, but uses custom lookup table
        -- TODO: custom base32 using https://hackage.haskell.org/package/base32-bytestring-0.2.1.0/docs/src/Data-ByteString-Base32.html#Base32
        --narHash <- ("sha256:" <>) . toS <$> readProcess "nix-hash" ["--type", "sha256", "--to-base32", toS narHashB64] mempty

        -- TODO: handle case if there is no deriver
        (exitcode, out, err) <- readProcessWithExitCode "nix-store" ["-q", "--deriver", toS storePath] mempty
        let deriver = T.strip $ toS out

        (exitcode, out, err) <- readProcessWithExitCode "nix-store" ["-q", "--references", toS storePath] mempty
        -- Upload narinfo with signature
        let references = T.lines $ toS out
            sig = dsign sk $ toS $ fingerprint storePath narHash narSize references
            nic = Api.NarInfoCreate
              { cStorePath = storePath
              , cNarHash = narHash
              , cNarSize = narSize
              , cReferences = references
              , cDeriver = deriver
              , cSig = toS $ B64.encode $ unSignature sig
              }
        return ()
        {-
        res <- (`runClientM` env) $ Api.createNarinfo (cachixBCClient name) nic
        case res of
          Left err -> panic $ show err
          Right NoContent -> return ()
        -}
  return ()

-- perl/lib/Nix/Manifest.pm:fingerprintPath
fingerprint :: Text -> Text -> Int -> [Text] -> Text
fingerprint storePath narHash narSize references = T.intercalate ";" $
  "1" : storePath : narHash : show narSize : (T.intercalate "," references) : []
