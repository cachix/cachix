{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Command.Config where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.OptionsParser (AuthTokenSource (..), SecretStore (..))
import Cachix.Client.Retry (retryClientM)
import Cachix.Client.SecretSpec qualified as SecretSpec
import Cachix.Client.Secrets
  ( SigningKey (SigningKey),
    exportSigningKey,
  )
import Cachix.Client.Servant
import Cachix.Types.SigningKeyCreate qualified as SigningKeyCreate
import Control.Exception.Safe qualified as Safe
import Crypto.Sign.Ed25519 (PublicKey (PublicKey), createKeypair)
import Data.ByteString.Base64 qualified as B64
import Data.String.Here
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API (NoContent (..))
import Servant.Auth.Client

-- | Pick the secret destination: honor an explicit flag, otherwise use
-- secretspec when it is configured on this machine. An unconfigured install
-- falls back to the Cachix configuration file with setup instructions.
resolveSecretStore :: SecretStore -> IO Bool
resolveSecretStore StoreSecretSpec = return True
resolveSecretStore StoreConfigFile = return False
resolveSecretStore StoreAuto = do
  configured <- SecretSpec.isConfigured
  if configured
    then return True
    else do
      when SecretSpec.supported $
        putErrText "Tip: run `secretspec config global init` to configure secretspec and store Cachix credentials in your password manager instead of a plaintext file."
      return False

-- TODO: check that token actually authenticates!
authtoken :: Env -> AuthTokenSource -> SecretStore -> IO ()
authtoken env source store = do
  useSecretspec <- resolveSecretStore store
  if useSecretspec
    then do
      maybeToken <- case source of
        TokenArg token -> return $ Just token
        TokenStdin -> Just . T.strip <$> T.IO.getContents
        -- the secretspec CLI prompts for the value with hidden input
        TokenPrompt -> return Nothing
      SecretSpec.setAuthToken maybeToken
      -- The configuration file takes precedence on reads, so a token left
      -- behind there would shadow the one just stored.
      clearConfigAuthToken env
    else do
      token <- case source of
        TokenArg token -> return token
        TokenStdin -> T.strip <$> T.IO.getContents
        TokenPrompt -> throwIO $ NoInput "Provide the auth token as an argument or via --stdin."
      let configPath = Config.configPath (cachixoptions env)
      config <- Config.getConfig configPath
      Config.writeConfig configPath $ config {Config.authToken = Token (toS token)}

clearConfigAuthToken :: Env -> IO ()
clearConfigAuthToken env = do
  let configPath = Config.configPath (cachixoptions env)
  config <- Config.getConfig configPath
  when (Config.authToken config /= Token "") $ do
    putStrLn ("Moving the auth token out of " <> toS configPath <> ", which would take precedence over secretspec." :: Text)
    Config.writeConfig configPath $ config {Config.authToken = Token ""}

generateKeypair :: Env -> Text -> SecretStore -> IO ()
generateKeypair env name store = do
  authToken <- Config.getAuthTokenRequired (config env)
  (PublicKey pk, sk) <- createKeypair
  let signingKey = exportSigningKey $ SigningKey sk
      signingKeyCreate = SigningKeyCreate.SigningKeyCreate (toS $ B64.encode pk)
      bcc = Config.BinaryCacheConfig name signingKey
  -- Validate the public key before changing the active local credential. If
  -- local storage subsequently fails, surface the private key below because
  -- this registration cannot be rolled back.
  (_ :: NoContent) <-
    escalate
      =<< retryClientM (clientenv env) (API.createKey cachixClient authToken name signingKeyCreate)

  useSecretspec <- resolveSecretStore store
  let storeSigningKey =
        if useSecretspec
          then do
            SecretSpec.setSigningKey name (Just signingKey)
            storeSecretspecCacheMetadata env name
          else do
            -- TODO: warn if binary cache with the same key already exists
            let cfg = config env & Config.setBinaryCaches [bcc]
            Config.writeConfig (Config.configPath (cachixoptions env)) cfg
  storeSigningKey `Safe.catchAny` \exception -> do
    putErrText
      ( [iTrim|
The public signing key was registered, but the private key could not be stored.
Save this signing key now; it is the only copy:

${signingKey}
  |] ::
          Text
      )
    throwIO exception

  if useSecretspec
    then do
      putStrLn
        ( [iTrim|
Secret signing key has been stored via secretspec under the "cachix" project
namespace. To populate your binary cache:

    $ nix-build | cachix push ${name}

To use the signing key on another machine or CI, configure the same secretspec
provider there, or export it:

    $ export CACHIX_SIGNING_KEY=<signing key...>

To instruct Nix to use the binary cache:

    $ cachix use ${name}

IMPORTANT: Make sure to make a backup for the signing key, as you have the only copy.
  |] ::
            Text
        )
    else do
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

storeSecretspecCacheMetadata :: Env -> Text -> IO ()
storeSecretspecCacheMetadata env cacheName = do
  let configPath = Config.configPath (cachixoptions env)
  cfg <- Config.getConfig configPath
  let matching = filter (\bc -> Config.name bc == cacheName) (Config.binaryCaches cfg)
      caches = secretspecCacheMetadata cacheName (Config.binaryCaches cfg)
  unless (all (T.null . Config.secretKey) matching) $
    putStrLn ("Moving the signing key for " <> cacheName <> " out of " <> toS configPath <> ", which would take precedence over secretspec." :: Text)
  Config.writeConfig configPath $ cfg {Config.binaryCaches = caches}

-- Retain the cache name for commands such as doctor, while ensuring the legacy
-- config file cannot shadow the key stored by secretspec. Existing duplicates
-- are collapsed at the same time.
secretspecCacheMetadata :: Text -> [Config.BinaryCacheConfig] -> [Config.BinaryCacheConfig]
secretspecCacheMetadata cacheName caches =
  filter (\bc -> Config.name bc /= cacheName) caches
    <> [Config.BinaryCacheConfig cacheName ""]
