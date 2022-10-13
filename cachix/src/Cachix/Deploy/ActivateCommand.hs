module Cachix.Deploy.ActivateCommand where

import qualified Cachix.API.Deploy as API
import Cachix.API.Error (escalate)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Env as Env
import Cachix.Client.Servant (deployClient)
import qualified Cachix.Client.URI as URI
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import qualified Cachix.Types.Deploy as Types
import qualified Cachix.Deploy.Websocket as WebSocket
import qualified Cachix.Types.DeployResponse as DeployResponse
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (filterWithKey)
import qualified Data.HashMap.Strict as HM
import qualified Data.UUID as UUID
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (runClientM)
import Servant.Conduit ()
import System.Environment (getEnv)

run :: Config.CachixOptions -> DeployOptions.ActivateOptions -> IO ()
run cachixOptions DeployOptions.ActivateOptions {DeployOptions.payloadPath, DeployOptions.agents} = do
  agentToken <- getEnv "CACHIX_ACTIVATE_TOKEN"
  clientEnv <- Env.createClientEnv cachixOptions
  payloadEither <- Aeson.eitherDecodeFileStrict' payloadPath

  case payloadEither of
    Left err -> do
      hPutStrLn stderr $ "Error while parsing JSON: " <> err
      exitFailure
    Right payload -> do
      let deploy =
            if not (null agents)
              then payload {Types.agents = filterWithKey (\k _ -> k `elem` agents) (Types.agents payload)}
              else payload
      response <- escalate <=< (`runClientM` clientEnv) $ API.activate deployClient (Token $ toS agentToken) deploy
      for_ (HM.toList $ DeployResponse.agents response) $
        \(agentName, details) -> do
          print details

          let deploymentID = DeployResponse.id details

          -- WebSocket.reconnectWithLog withLog $
          let host = Config.host cachixOptions
          let headers = [("Authorization", "Bearer " <> toS agentToken)]
          let port = fromMaybe (URI.Port 80) (URI.getPortFor (URI.getScheme host))
          let options =
                WebSocket.Options
                  { WebSocket.host = URI.getHostname host,
                    WebSocket.port = port,
                    WebSocket.path = "/api/v1/deploy/log/" <> UUID.toText deploymentID <> "?view=true",
                    WebSocket.useSSL = URI.requiresSSL (URI.getScheme host),
                    WebSocket.headers = headers,
                    WebSocket.identifier = ""
                  }

          WebSocket.runClientWith options WS.defaultConnectionOptions $ \connection ->
            forever $ do
              message <- WS.receiveData connection
              case Aeson.eitherDecodeStrict' message of
                Left error -> print error
                Right msg -> putStrLn $ unwords [inSquareBrackets agentName, WSS.line msg]

inSquareBrackets s = "[" <> s <> "]"
