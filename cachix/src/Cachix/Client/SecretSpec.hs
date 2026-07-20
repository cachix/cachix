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
-- secret stored once under that namespace is found from any directory.
--
-- Available when cachix is built with the @secretspec@ cabal flag; without it
-- 'getSecret' resolves nothing and 'setSecret' fails.
module Cachix.Client.SecretSpec
  ( supported,
    getSecret,
    setSecret,
  )
where

import Cachix.Client.Exception (CachixException (..))
import Protolude hiding (toS)
#ifdef USE_SECRETSPEC
import Control.Exception.Safe qualified as Safe
import Data.Map.Strict qualified as Map
import Data.String.Here
import Protolude.Conv
import SecretSpec qualified
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import System.IO.Temp qualified as Temp
import System.Process qualified as Process
#endif

-- | Whether this build of cachix was compiled with secretspec support.
supported :: Bool

-- | Look up a secret by its declared name, first in the project's
-- secretspec.toml, then under the embedded "cachix" project namespace. Any
-- resolution failure (no manifest, provider error, missing required secrets)
-- falls through to Nothing.
getSecret :: Text -> IO (Maybe Text)

-- | Store a secret in the user's default provider under the embedded "cachix"
-- project namespace by invoking the secretspec CLI, which is the only write
-- path: the linked resolver is read-only.
setSecret :: Text -> Text -> IO ()

#ifdef USE_SECRETSPEC
supported = True

getSecret name = do
  projectSecret <- resolveSecret name identity
  case projectSecret of
    Just value -> return $ Just value
    Nothing ->
      withEmbeddedManifest $ \manifestPath ->
        resolveSecret name (SecretSpec.withPath (toS manifestPath))

setSecret name value = do
  maybeExecutable <- findExecutable "secretspec"
  case maybeExecutable of
    Nothing ->
      throwIO $ SecretSpecError "Storing secrets requires the secretspec executable. Install it from https://secretspec.dev and try again."
    Just executable ->
      withEmbeddedManifest $ \manifestPath -> do
        -- Access reasons are required by the default require_reason policy in
        -- agent environments; a SECRETSPEC_REASON the user exported is read by
        -- the CLI itself and should not be overridden.
        maybeReason <- lookupEnv "SECRETSPEC_REASON"
        let reasonArgs = maybe ["--reason", "cachix credential store"] (const []) maybeReason
        -- rawSystem inherits stdio, so provider prompts and confirmations from
        -- the CLI reach the user, and unlike callProcess a failure does not
        -- echo the argument vector holding the secret value.
        exitCode <- Process.rawSystem executable (["--file", manifestPath, "set", toS name, toS value] <> reasonArgs)
        case exitCode of
          ExitSuccess -> return ()
          ExitFailure code ->
            throwIO $ SecretSpecError $ "secretspec set " <> name <> " failed with exit code " <> show code

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
withEmbeddedManifest :: (FilePath -> IO a) -> IO a
withEmbeddedManifest action =
  Temp.withSystemTempDirectory "cachix-secretspec" $ \dir -> do
    let manifestPath = dir </> "secretspec.toml"
    writeFile manifestPath embeddedManifest
    action manifestPath

-- | The global fallback manifest compiled into the binary. The project name
-- namespaces the secrets in the user's default provider, shared by every
-- directory cachix runs from.
embeddedManifest :: Text
embeddedManifest =
  [iTrim|
[project]
name = "cachix"
revision = "1.0"

[profiles.default]
CACHIX_AUTH_TOKEN = { description = "Cachix auth token from https://app.cachix.org", required = false }
CACHIX_SIGNING_KEY = { description = "Cachix binary cache signing key", required = false }
  |]
#else
supported = False

getSecret _ = return Nothing

setSecret _ _ =
  throwIO $ SecretSpecError "This cachix binary was built without secretspec support (cabal flag \"secretspec\")."
#endif
