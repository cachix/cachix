{-# LANGUAGE CPP #-}
#ifdef USE_SECRETSPEC
{-# LANGUAGE QuasiQuotes #-}
#endif

-- | Integration with secretspec (https://secretspec.dev), a declarative
-- secrets manager.
--
-- Secrets are resolved from the project's secretspec.toml (walking up from the
-- working directory) and, failing that, from a manifest embedded in the binary
-- whose project name is "cachix". Providers namespace secrets by project name
-- (e.g. the keyring entry secretspec/cachix/default/CACHIX_AUTH_TOKEN), so a
-- secret stored once under that namespace is found from any directory. Signing
-- keys are per cache: the embedded manifest addresses them through a secret
-- reference with item CACHIX_SIGNING_KEY_<cache>, so each cache gets its own
-- slot in the provider.
--
-- Available when cachix is built with the @secretspec@ cabal flag; without it
-- resolution finds nothing, 'isConfigured' is False, and storing fails.
module Cachix.Client.SecretSpec
  ( supported,
    cliAvailable,
    isConfigured,
    configInit,
    getAuthToken,
    getSigningKey,
    setAuthToken,
    setSigningKey,
  )
where

import Cachix.Client.Exception (CachixException (..))
import Protolude hiding (toS)
#ifdef USE_SECRETSPEC
import Control.Exception.Safe qualified as Safe
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.String.Here
import Data.Text qualified as T
import Protolude.Conv
import SecretSpec qualified
import System.Directory (XdgDirectory (..), doesFileExist, findExecutable, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import System.IO.Temp qualified as Temp
import System.Process qualified as Process
#endif

-- | Whether this build of cachix was compiled with secretspec support.
supported :: Bool

-- | Whether the secretspec CLI is on PATH.
cliAvailable :: IO Bool

-- | Whether secretspec can be used as a credential store on this machine: the
-- build supports it, the secretspec CLI is on PATH, and a default provider is
-- configured (SECRETSPEC_PROVIDER or the user configuration file).
isConfigured :: IO Bool

-- | Run @secretspec config init@ interactively (inheriting the terminal) to
-- let the user pick a default provider. Returns whether it succeeded.
configInit :: IO Bool

-- | Resolve CACHIX_AUTH_TOKEN, first from the project's secretspec.toml, then
-- from the embedded "cachix" namespace. Any resolution failure (no manifest,
-- provider error, missing required secrets) falls through to Nothing.
getAuthToken :: IO (Maybe Text)

-- | Resolve CACHIX_SIGNING_KEY for the given cache, first from the project's
-- secretspec.toml, then from the per cache slot in the embedded namespace.
getSigningKey :: Text -> IO (Maybe Text)

-- | Store CACHIX_AUTH_TOKEN in the user's default provider under the embedded
-- "cachix" namespace by invoking the secretspec CLI, which is the only write
-- path: the linked resolver is read-only. Passing Nothing lets the CLI prompt
-- for the value securely (hidden input; requires a terminal).
setAuthToken :: Maybe Text -> IO ()

-- | Store CACHIX_SIGNING_KEY for the given cache in its per cache slot.
setSigningKey :: Text -> Maybe Text -> IO ()

#ifdef USE_SECRETSPEC
supported = True

cliAvailable = isJust <$> findExecutable "secretspec"

isConfigured = do
  maybeExecutable <- findExecutable "secretspec"
  case maybeExecutable of
    Nothing -> return False
    Just _ -> do
      maybeProvider <- lookupEnv "SECRETSPEC_PROVIDER"
      case maybeProvider of
        Just _ -> return True
        Nothing -> doesFileExist =<< getXdgDirectory XdgConfig ("secretspec" </> "config.toml")

configInit = do
  maybeExecutable <- findExecutable "secretspec"
  case maybeExecutable of
    Nothing -> return False
    Just executable -> do
      exitCode <- Process.rawSystem executable ["config", "init"]
      return $ exitCode == ExitSuccess

getAuthToken = getSecret AuthTokenSecret

getSigningKey cacheName = getSecret (SigningKeySecret cacheName)

setAuthToken = setSecret AuthTokenSecret

setSigningKey cacheName = setSecret (SigningKeySecret cacheName)

-- | The two credentials cachix manages through the embedded manifest.
data EmbeddedSecret
  = AuthTokenSecret
  | SigningKeySecret Text

-- | The declared name, which is also what project manifests declare and what
-- resolution looks up in the resolved set.
secretName :: EmbeddedSecret -> Text
secretName AuthTokenSecret = "CACHIX_AUTH_TOKEN"
secretName (SigningKeySecret _) = "CACHIX_SIGNING_KEY"

getSecret :: EmbeddedSecret -> IO (Maybe Text)
getSecret secret = do
  projectSecret <- resolveSecret (secretName secret) identity
  case projectSecret of
    Just value -> return $ Just value
    Nothing ->
      -- The embedded namespace is profile-independent: the manifest declares
      -- only [profiles.default], and pinning the profile keeps a stored
      -- credential reachable whatever SECRETSPEC_PROFILE or the user's
      -- configured default profile happen to be.
      withEmbeddedManifest secret $ \manifestPath ->
        resolveSecret (secretName secret) (SecretSpec.withPath (toS manifestPath) . SecretSpec.withProfile "default")

setSecret :: EmbeddedSecret -> Maybe Text -> IO ()
setSecret secret maybeValue = do
  maybeExecutable <- findExecutable "secretspec"
  case maybeExecutable of
    Nothing ->
      throwIO $ SecretSpecError "Storing secrets requires the secretspec executable. Install it from https://secretspec.dev and try again."
    Just executable ->
      withEmbeddedManifest secret $ \manifestPath -> do
        -- Access reasons are required by the default require_reason policy in
        -- agent environments; a SECRETSPEC_REASON the user exported is read by
        -- the CLI itself and should not be overridden.
        maybeReason <- lookupEnv "SECRETSPEC_REASON"
        let reasonArgs = maybe ["--reason", "cachix credential store"] (const []) maybeReason
        -- Omitting the value makes the CLI prompt for it with hidden input.
        let valueArgs = maybeToList (toS <$> maybeValue)
        -- rawSystem inherits stdio, so the CLI's prompts and confirmations
        -- reach the user, and unlike callProcess a failure does not echo the
        -- argument vector holding the secret value.
        -- The profile is pinned for the same reason resolution pins it: the
        -- embedded manifest only declares [profiles.default].
        exitCode <- Process.rawSystem executable (["--file", manifestPath, "set", toS (secretName secret)] <> valueArgs <> ["--profile", "default"] <> reasonArgs)
        case exitCode of
          ExitSuccess -> return ()
          ExitFailure code ->
            throwIO $ SecretSpecError $ "secretspec set " <> secretName secret <> " failed with exit code " <> show code

-- | Run one resolution and extract the named secret, mapping every failure to
-- Nothing.
resolveSecret :: Text -> (SecretSpec.Builder -> SecretSpec.Builder) -> IO (Maybe Text)
resolveSecret name adjustBuilder =
  Safe.catches
    resolve
    [ Safe.Handler $ \(_ :: SecretSpec.SecretSpecError) -> return Nothing,
      Safe.Handler $ \(_ :: SecretSpec.MissingRequiredError) -> return Nothing
    ]
  where
    resolve = do
      resolved <- SecretSpec.load (adjustBuilder (SecretSpec.withReason "cachix credential lookup" SecretSpec.builder))
      let maybeValue = SecretSpec.secretValue =<< Map.lookup name (SecretSpec.resolvedSecrets resolved)
      SecretSpec.close resolved
      case maybeValue of
        Just value | value /= "" -> return $ Just value
        _ -> return Nothing

-- | Materialize the embedded manifest for the duration of the action; the
-- resolver and the CLI only read manifests from disk.
withEmbeddedManifest :: EmbeddedSecret -> (FilePath -> IO a) -> IO a
withEmbeddedManifest secret action =
  Temp.withSystemTempDirectory "cachix-secretspec" $ \dir -> do
    let manifestPath = dir </> "secretspec.toml"
    writeFile manifestPath (embeddedManifest secret)
    action manifestPath

-- | The global fallback manifest compiled into the binary. The project name
-- namespaces the secrets in the user's default provider, shared by every
-- directory cachix runs from. The signing key entry uses a secret reference so
-- each cache stores under its own item instead of one shared slot.
embeddedManifest :: EmbeddedSecret -> Text
embeddedManifest AuthTokenSecret =
  [iTrim|
[project]
name = "cachix"
revision = "1.0"

[profiles.default]
CACHIX_AUTH_TOKEN = { description = "Cachix auth token from https://app.cachix.org", required = false }
  |]
embeddedManifest (SigningKeySecret cacheName) =
  [iTrim|
[project]
name = "cachix"
revision = "1.0"

[profiles.default]
CACHIX_SIGNING_KEY = { description = "Cachix binary cache signing key", required = false, ref = { item = ${itemName} } }
  |]
  where
    -- The item doubles as a key in env-shaped stores (the dotenv provider
    -- rejects names with dashes on read), so the cache name is folded into a
    -- valid environment identifier. show adds the TOML quoting.
    itemName :: Text
    itemName = show ("CACHIX_SIGNING_KEY_" <> T.map sanitize (T.toUpper cacheName) :: Text)

    sanitize :: Char -> Char
    sanitize c = if Char.isAlphaNum c then c else '_'
#else
supported = False

cliAvailable = return False

isConfigured = return False

configInit = return False

getAuthToken = return Nothing

getSigningKey _ = return Nothing

setAuthToken _ = throwIO unsupportedError

setSigningKey _ _ = throwIO unsupportedError

unsupportedError :: CachixException
unsupportedError = SecretSpecError "This cachix binary was built without secretspec support (cabal flag \"secretspec\")."
#endif
