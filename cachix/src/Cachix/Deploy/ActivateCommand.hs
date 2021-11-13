{-# LANGUAGE NamedFieldPuns #-}

module Cachix.Deploy.ActivateCommand where

import qualified Cachix.API.Deploy as API
import Cachix.API.Error (escalate)
import qualified Cachix.Client.Env as Env
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.Servant (deployClient)
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import qualified Cachix.Types.DeployResponse as DeployResponse
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (runClientM)
import Servant.Conduit ()
import System.Environment (getEnv)

run :: CachixOptions.CachixOptions -> DeployOptions.ActivateOptions -> IO ()
run cachixOptions DeployOptions.ActivateOptions {DeployOptions.payloadPath} = do
  agentToken <- getEnv "CACHIX_DEPLOY_TOKEN"
  clientEnv <- Env.createClientEnv cachixOptions
  payloadEither <- Aeson.eitherDecodeFileStrict' payloadPath
  case payloadEither of
    Left err -> do
      hPutStrLn stderr $ "Error while parsing JSON: " <> err
      exitFailure
    Right payload -> do
      response <- escalate <=< (`runClientM` clientEnv) $ API.activate deployClient (Token $ toS agentToken) payload
      for_ (HM.toList $ DeployResponse.agents response) $
        \(_, url) -> putStrLn url
