{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.ActivateCommand where

import qualified Cachix.API.Deploy as API
import Cachix.API.Error (escalate)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Env as Env
import Cachix.Client.Servant (deployClient)
import qualified Cachix.Client.URI as URI
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import qualified Cachix.Deploy.Websocket as WebSocket
import Cachix.Types.Deploy (Deploy)
import qualified Cachix.Types.Deploy as Types
import qualified Cachix.Types.DeployResponse as DeployResponse
import qualified Cachix.Types.Deployment as Deployment
import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (filterWithKey)
import qualified Data.HashMap.Strict as HM
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (ClientEnv, runClientM)
import Servant.Conduit ()
import System.Environment (getEnv)

run :: Env.Env -> DeployOptions.ActivateOptions -> IO ()
run env DeployOptions.ActivateOptions {DeployOptions.payloadPath, DeployOptions.agents} = do
  agentToken <- toS <$> getEnv "CACHIX_ACTIVATE_TOKEN"
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
      activate env agentToken deploy

activate :: Env.Env -> ByteString -> Deploy -> IO ()
activate Env.Env {cachixoptions, clientenv} agentToken payload = do
  response <- escalate <=< (`runClientM` clientenv) $ API.activate deployClient (Token $ toS agentToken) payload
  let agents = HM.toList (DeployResponse.agents response)
  for_ agents $ \(agentName, details) -> do
    putStrLn $
      unlines
        [ "Deploying " <> agentName,
          DeployResponse.url details
        ]

    let deploymentID = DeployResponse.id details

    let host = Config.host cachixoptions
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

    Async.race_
      (printLogsToTerminal options agentName)
      (pollDeploymentStatus clientenv (Token $ toS agentToken) deploymentID)

pollDeploymentStatus :: ClientEnv -> Token -> UUID -> IO ()
pollDeploymentStatus clientEnv token deploymentID = loop
  where
    loop = do
      response <- escalate <=< (`runClientM` clientEnv) $ API.getDeployment deployClient token deploymentID
      case Deployment.status response of
        Deployment.Cancelled -> putStrLn ("cancelled" :: Text)
        Deployment.Failed -> putStrLn ("failed" :: Text)
        Deployment.Succeeded -> print ("succeeded" :: Text)
        _ -> do
          threadDelay (2 * 1000 * 1000)
          loop

printLogsToTerminal :: WebSocket.Options -> Text -> IO a
printLogsToTerminal options agentName =
  WebSocket.runClientWith options WS.defaultConnectionOptions $ \connection ->
    forever $ do
      message <- WS.receiveData connection
      case Aeson.eitherDecodeStrict' message of
        Left error -> print error
        Right msg -> putStrLn $ unwords [inSquareBrackets agentName, WSS.line msg]

inSquareBrackets :: (Semigroup a, IsString a) => a -> a
inSquareBrackets s = "[" <> s <> "]"
