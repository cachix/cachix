{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Command.Config where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (AccessDeniedBinaryCache))
import Cachix.Client.Retry (retryClientM)
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
import Network.HTTP.Types.Status (status403)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API (NoContent (..))
import Servant.Auth.Client

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
  createKeyResult <- retryClientM (clientenv env) (API.createKey cachixClient authToken name signingKeyCreate)
  (_ :: NoContent) <-
    case createKeyResult of
      Left err
        | isErr err status403 -> throwIO $ generateKeypairAccessDenied name
      _ -> escalate createKeyResult
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

generateKeypairAccessDenied :: Text -> CachixException
generateKeypairAccessDenied name =
  AccessDeniedBinaryCache
    [iTrim|
Cannot create a signing key for binary cache ${name}.

This operation requires a personal auth token belonging to a Cachix account that administers the cache. Per-cache read/write tokens can push and pull, but they can't manage signing keys.
    |]
