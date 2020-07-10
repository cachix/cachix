{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Commands
  ( authtoken,
    create,
    generateKeypair,
    push,
    use,
  )
where

import qualified Cachix.Api as Api
import Cachix.Api.Error
import Cachix.Client.Config
  ( BinaryCacheConfig (BinaryCacheConfig),
    Config (..),
    mkConfig,
    writeConfig,
  )
import qualified Cachix.Client.Config as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.HumanSize (humanSize)
import qualified Cachix.Client.InstallationMode as InstallationMode
import qualified Cachix.Client.NixConf as NixConf
import Cachix.Client.NixVersion (assertNixVersion)
import Cachix.Client.OptionsParser
  ( CachixOptions (..),
    PushArguments (..),
    PushOptions (..),
  )
import Cachix.Client.Push
import Cachix.Client.Secrets
  ( SigningKey (SigningKey),
    exportSigningKey,
    parseSigningKeyLenient,
  )
import Cachix.Client.Servant
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import Control.Exception.Safe (handle, throwM)
import Control.Retry (RetryStatus (rsIterNumber))
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.List (isSuffixOf)
import Data.String.Here
import qualified Data.Text as T
import Network.HTTP.Types (status401, status404)
import Protolude
import Servant.API (NoContent)
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FSNotify
import System.IO (hIsTerminalDevice)

authtoken :: Env -> Text -> IO ()
authtoken env token = do
  -- TODO: check that token actually authenticates!
  writeConfig (configPath (cachixoptions env)) $ case config env of
    Just cfg -> cfg {authToken = Token (toS token)}
    Nothing -> mkConfig token

create :: Env -> Text -> IO ()
create _ _ =
  throwIO $ DeprecatedCommand "Create command has been deprecated. Please visit https://cachix.org to create a binary cache."

generateKeypair :: Env -> Text -> IO ()
generateKeypair Env {config = Nothing} _ = throwIO $ NoConfig "Start with visiting https://cachix.org and copying the token to $ cachix authtoken <token>"
generateKeypair env@Env {config = Just cfg} name = do
  (PublicKey pk, sk) <- createKeypair
  let signingKey = exportSigningKey $ SigningKey sk
      signingKeyCreate = SigningKeyCreate.SigningKeyCreate (toS $ B64.encode pk)
      bcc = BinaryCacheConfig name signingKey
  -- we first validate if key can be added to the binary cache
  (_ :: NoContent) <-
    escalate <=< (`runClientM` clientenv env) $
      Api.createKey (cachixBCClient name) (authToken cfg) signingKeyCreate
  -- if key was successfully added, write it to the config
  -- TODO: warn if binary cache with the same key already exists
  writeConfig (configPath (cachixoptions env)) $
    cfg {binaryCaches = binaryCaches cfg <> [bcc]}
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

envToToken :: Env -> Token
envToToken env =
  maybe (Token "") authToken (config env)

notAuthenticatedBinaryCache :: Text -> CachixException
notAuthenticatedBinaryCache name =
  AccessDeniedBinaryCache $
    "Binary cache " <> name <> " doesn't exist or it's private and you must first authenticate using:  $ cachix authtoken <token>"

accessDeniedBinaryCache :: Text -> CachixException
accessDeniedBinaryCache name =
  AccessDeniedBinaryCache $ "Binary cache " <> name <> " doesn't exist or it's private and you don't have access it"

use :: Env -> Text -> InstallationMode.UseOptions -> IO ()
use env name useOptions = do
  -- 1. get cache public key
  res <- (`runClientM` clientenv env) $ Api.get (cachixBCClient name) (envToToken env)
  case res of
    Left err
      | isErr err status401 && isJust (config env) -> throwM $ accessDeniedBinaryCache name
      | isErr err status401 -> throwM $ notAuthenticatedBinaryCache name
      | isErr err status404 -> throwM $ BinaryCacheNotFound $ "Binary cache" <> name <> " does not exist."
      | otherwise -> throwM err
    Right binaryCache -> do
      () <- escalateAs UnsupportedNixVersion =<< assertNixVersion
      user <- InstallationMode.getUser
      nc <- NixConf.read NixConf.Global
      isTrusted <- InstallationMode.isTrustedUser $ NixConf.readLines (catMaybes [nc]) NixConf.isTrustedUsers
      isNixOS <- doesFileExist "/etc/NIXOS"
      let nixEnv =
            InstallationMode.NixEnv
              { InstallationMode.isRoot = user == "root",
                InstallationMode.isTrusted = isTrusted,
                InstallationMode.isNixOS = isNixOS
              }
      InstallationMode.addBinaryCache (config env) binaryCache useOptions $
        fromMaybe (InstallationMode.getInstallationMode nixEnv) (InstallationMode.useMode useOptions)

-- TODO: lots of room for performance improvements
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
  sk <- findSigningKey env name
  store <- wait (storeAsync env)
  void $
    pushClosure
      (mapConcurrentlyBounded (numJobs opts))
      (clientenv env)
      store
      PushCache
        { pushCacheToken = envToToken env,
          pushCacheName = name,
          pushCacheSigningKey = sk
        }
      (pushStrategy env opts name)
      inputStorePaths
  putText "All done."
push env (PushWatchStore opts name) = withManager $ \mgr -> do
  _ <- watchDir mgr "/nix/store" filterF action
  _ <- wait (storeAsync env)
  putText "Watching /nix/store for new builds ..."
  forever $ threadDelay 1000000
  where
    logErr :: FilePath -> SomeException -> IO ()
    logErr fp e = hPutStrLn stderr $ "Exception occured while pushing " <> fp <> ": " <> show e
    action :: Action
    action (Removed fp _ _) =
      Control.Exception.Safe.handle (logErr fp)
        $ pushStorePath env opts name
        $ toS
        $ dropEnd 5 fp
    action _ = return ()
    filterF :: ActionPredicate
    filterF (Removed fp _ _)
      | ".lock" `isSuffixOf` fp = True
    filterF _ = False
    dropEnd :: Int -> [a] -> [a]
    dropEnd index xs = take (length xs - index) xs

-- | Find a secret key in the 'Config' or environment variable
findSigningKey ::
  Env ->
  -- | Cache name
  Text ->
  -- | Secret key or exception
  IO SigningKey
findSigningKey env name = do
  maybeEnvSK <- lookupEnv "CACHIX_SIGNING_KEY"
  -- we reverse list of caches to prioritize keys added as last
  let matches c = filter (\bc -> Config.name bc == name) $ reverse $ binaryCaches c
      maybeBCSK = case config env of
        Nothing -> Nothing
        Just c -> Config.secretKey <$> head (matches c)
  signingKey <- case maybeBCSK <|> toS <$> maybeEnvSK of
    Just key -> return key
    Nothing -> throwIO $ NoSigningKey msg
  escalateAs FatalError $ parseSigningKeyLenient signingKey
  where
    msg :: Text
    msg =
      [iTrim|
Signing key not found. 

It is generated by `cachix generate-keypair <name>` and stored in ~/.config/cachix/cachix.dhall

There are a few options why this happened:

a) You haven't generated signing key yet for your cache 

b) You have the key but you're pushing from a different machine. 
   You can set it via $CACHIX_SIGNING_KEY environment variable.

c) You've lost the signing key. In that case it's best to delete the cache and start again.
   Note that everyone that configured the binary cache will have to do it again to set the new
   public key.
      |]

retryText :: RetryStatus -> Text
retryText retrystatus =
  if rsIterNumber retrystatus == 0
    then ""
    else "(retry #" <> show (rsIterNumber retrystatus) <> ") "

pushStrategy :: Env -> PushOptions -> Text -> Text -> PushStrategy IO ()
pushStrategy env opts name storePath =
  PushStrategy
    { onAlreadyPresent = pass,
      on401 =
        if isJust (config env)
          then throwM $ accessDeniedBinaryCache name
          else throwM $ notAuthenticatedBinaryCache name,
      onError = throwM,
      onAttempt = \retrystatus size ->
        -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
        putStr $ retryText retrystatus <> "compressing and pushing " <> storePath <> " (" <> humanSize (fromIntegral size) <> ")\n",
      onDone = pass,
      withXzipCompressor = defaultWithXzipCompressorWithLevel (compressionLevel opts),
      Cachix.Client.Push.omitDeriver = Cachix.Client.OptionsParser.omitDeriver opts
    }

pushStorePath :: Env -> PushOptions -> Text -> Text -> IO ()
pushStorePath env opts name storePath = do
  sk <- findSigningKey env name
  -- use secret key from config or env
  -- TODO: this shouldn't happen for each store path
  store <- wait (storeAsync env)
  pushSingleStorePath
    (clientenv env)
    store
    PushCache
      { pushCacheToken = envToToken env,
        pushCacheName = name,
        pushCacheSigningKey = sk
      }
    (pushStrategy env opts name storePath)
    storePath
