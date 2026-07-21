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
-- secretspec exactly when it is usable on this machine.
resolveSecretStore :: SecretStore -> IO Bool
resolveSecretStore StoreSecretSpec = return True
resolveSecretStore StoreConfigFile = return False
resolveSecretStore StoreAuto = do
  configured <- SecretSpec.isConfigured
  when (SecretSpec.supported && not configured) $
    putErrText "Tip: configure secretspec (https://secretspec.dev) to store cachix credentials in your password manager instead of a plaintext file."
  return configured

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
  useSecretspec <- resolveSecretStore store
  authToken <- Config.getAuthTokenRequired (config env)
  (PublicKey pk, sk) <- createKeypair
  let signingKey = exportSigningKey $ SigningKey sk
      signingKeyCreate = SigningKeyCreate.SigningKeyCreate (toS $ B64.encode pk)
      bcc = Config.BinaryCacheConfig name signingKey
  -- we first validate if key can be added to the binary cache
  (_ :: NoContent) <-
    escalate
      =<< retryClientM (clientenv env) (API.createKey cachixClient authToken name signingKeyCreate)
  -- if key was successfully added, store it locally
  if useSecretspec
    then do
      SecretSpec.setSigningKey name (Just signingKey)
      -- A stale key for this cache in the configuration file would take
      -- precedence over the one just stored and no longer match the cache.
      clearConfigSigningKey env name
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

clearConfigSigningKey :: Env -> Text -> IO ()
clearConfigSigningKey env cacheName = do
  let configPath = Config.configPath (cachixoptions env)
  cfg <- Config.getConfig configPath
  let remaining = filter (\bc -> Config.name bc /= cacheName) (Config.binaryCaches cfg)
  when (length remaining /= length (Config.binaryCaches cfg)) $ do
    putStrLn ("Moving the signing key for " <> cacheName <> " out of " <> toS configPath <> ", which would take precedence over secretspec." :: Text)
    Config.writeConfig configPath $ cfg {Config.binaryCaches = remaining}
