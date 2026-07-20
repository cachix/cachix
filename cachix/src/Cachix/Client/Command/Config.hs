{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Command.Config where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser (UseSecretSpec)
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

-- TODO: check that token actually authenticates!
authtoken :: Env -> Maybe Text -> UseSecretSpec -> IO ()
authtoken Env {cachixoptions} (Just token) useSecretspec
  | useSecretspec = SecretSpec.setSecret "CACHIX_AUTH_TOKEN" token
  | otherwise = do
      let configPath = Config.configPath cachixoptions
      config <- Config.getConfig configPath
      Config.writeConfig configPath $ config {Config.authToken = Token (toS token)}
authtoken env Nothing useSecretspec =
  (\token -> authtoken env (Just (T.strip token)) useSecretspec) =<< T.IO.getContents

generateKeypair :: Env -> Text -> UseSecretSpec -> IO ()
generateKeypair env name useSecretspec = do
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
      SecretSpec.setSecret "CACHIX_SIGNING_KEY" signingKey
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
