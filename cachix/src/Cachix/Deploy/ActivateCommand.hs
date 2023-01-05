module Cachix.Deploy.ActivateCommand where

import qualified Cachix.API.Deploy as API
import Cachix.API.Error (escalate)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Env as Env
import qualified Cachix.Client.Retry as Retry
import Cachix.Client.Servant (deployClient)
import qualified Cachix.Client.URI as URI
import Cachix.Client.Version (versionNumber)
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
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
  -- TODO: improve the error message here
  agentToken <- toS <$> getEnv "CACHIX_ACTIVATE_TOKEN"
  Aeson.eitherDecodeFileStrict' payloadPath >>= \case
    Left err -> do
      hPutStrLn stderr $ "Error parsing the deployment spec: " <> err
      exitFailure
    Right deploySpec -> do
      activate env agentToken (filterAgents agents deploySpec)
  where
    filterAgents [] deploySpec = deploySpec
    filterAgents chosenAgents deploySpec =
      deploySpec
        { Types.agents = filterWithKey (\k _ -> k `elem` chosenAgents) (Types.agents deploySpec)
        }

-- TODO: use prettyprinter
activate :: Env.Env -> ByteString -> Deploy -> IO ()
activate Env.Env {cachixoptions, clientenv} agentToken payload = do
  deployResponse <-
    escalate <=< (`runClientM` clientenv) $
      API.activate deployClient (Token agentToken) payload

  let agents = HM.toList (DeployResponse.agents deployResponse)

  Text.putStr (renderOverview agents)
  Text.putStr "\n\n"

  deployments <- Async.mapConcurrently watchDeployments agents

  Text.putStr "\n"
  Text.putStr (renderSummary deployments)

  if all isSuccessfulDeployment deployments
    then exitSuccess
    else exitFailure
  where
    isSuccessfulDeployment = (==) Deployment.Succeeded . Deployment.status . snd

    watchDeployments (agentName, details) = do
      let deploymentID = DeployResponse.id details
          host = Config.host cachixoptions
          hostname = URI.getHostname host
          port = fromMaybe (URI.Port 80) (URI.getPortFor (URI.getScheme host))
          path = "/api/v1/deploy/log/" <> UUID.toText deploymentID <> "?view=true"
          useSSL = URI.requiresSSL (URI.getScheme host)
          headers = [("Authorization", "Bearer " <> agentToken)]
          identifier = unwords ["cachix", versionNumber]
          options =
            WebSocket.Options
              { WebSocket.host = hostname,
                WebSocket.port = port,
                WebSocket.path = path,
                WebSocket.useSSL = useSSL,
                WebSocket.headers = headers,
                WebSocket.identifier = identifier
              }

      deployment <-
        Async.withAsync (printLogsToTerminal options agentName) $ \_ ->
          pollDeploymentStatus clientenv (Token agentToken) deploymentID

      pure (agentName, deployment)

pollDeploymentStatus :: ClientEnv -> Token -> UUID -> IO Deployment.Deployment
pollDeploymentStatus clientEnv token deploymentID = loop
  where
    loop = do
      deployment <-
        Retry.retryAll . const $
          escalate <=< (`runClientM` clientEnv) $
            API.getDeployment deployClient token deploymentID

      case Deployment.status deployment of
        Deployment.Cancelled -> pure deployment
        Deployment.Failed -> pure deployment
        Deployment.Succeeded -> pure deployment
        _ -> do
          threadDelay (2 * 1000 * 1000)
          loop

printLogsToTerminal :: WebSocket.Options -> Text -> IO a
printLogsToTerminal options agentName =
  WebSocket.runClientWith options WS.defaultConnectionOptions $ \connection ->
    forever $ do
      message <- WS.receiveData connection
      case Aeson.eitherDecodeStrict' message of
        Left error -> Text.putStrLn $ "Error parsing the log message: " <> show error
        Right msg -> Text.putStrLn $ unwords [inBrackets agentName, WSS.line msg]

renderOverview :: [(Text, DeployResponse.Details)] -> Text
renderOverview agents =
  Text.intercalate "\n" $
    "Deploying agents:" :
      [ inBrackets agentName <> " " <> DeployResponse.url details
        | (agentName, details) <- agents
      ]

renderSummary :: [(Text, Deployment.Deployment)] -> Text
renderSummary results =
  Text.intercalate "\n" $
    "Deployment summary:" :
      [ inBrackets agentName <> " " <> renderStatus (Deployment.status deployment)
        | (agentName, deployment) <- results
      ]
  where
    renderStatus = \case
      Deployment.Succeeded -> "Deployed successfully"
      Deployment.Failed -> "Failed to deploy"
      Deployment.Cancelled -> "Deployment cancelled"
      Deployment.InProgress -> "Still deploying"
      Deployment.Pending -> "Deployment not started"

inBrackets :: (Semigroup a, IsString a) => a -> a
inBrackets s = "[" <> s <> "]"
