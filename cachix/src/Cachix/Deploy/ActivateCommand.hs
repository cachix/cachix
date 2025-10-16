module Cachix.Deploy.ActivateCommand where

import Cachix.API.Deploy.V1 qualified as API.V1
import Cachix.API.Deploy.V2 qualified as API.V2
import Cachix.API.Error (escalate)
import Cachix.API.WebSocketSubprotocol qualified as WSS
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env qualified as Env
import Cachix.Client.Retry qualified as Retry
import Cachix.Client.Servant (deployClientV1, deployClientV2)
import Cachix.Client.URI qualified as URI
import Cachix.Client.Version (versionNumber)
import Cachix.Deploy.OptionsParser qualified as DeployOptions
import Cachix.Deploy.Websocket qualified as WebSocket
import Cachix.Types.Deploy (Deploy)
import Cachix.Types.Deploy qualified as Types
import Cachix.Types.DeployResponse qualified as DeployResponse
import Cachix.Types.Deployment qualified as Deployment
import Control.Concurrent.Async qualified as Async
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict (filterWithKey)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.WebSockets qualified as WS
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (ClientEnv, runClientM)
import Servant.Conduit ()
import System.Environment (getEnv)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse

run :: Env.Env -> DeployOptions.ActivateOptions -> IO ()
run env DeployOptions.ActivateOptions {DeployOptions.payloadPath, DeployOptions.agents, DeployOptions.deployAsync} = do
  -- TODO: improve the error message here
  agentToken <- toS <$> getEnv "CACHIX_ACTIVATE_TOKEN"
  Aeson.eitherDecodeFileStrict' payloadPath >>= \case
    Left err -> do
      putErrText $ "Error parsing the deployment spec: " <> toS err
      exitFailure
    Right deploySpec -> do
      activate env deployAsync agentToken (filterAgents agents deploySpec)
  where
    filterAgents [] deploySpec = deploySpec
    filterAgents chosenAgents deploySpec =
      deploySpec
        { Types.agents = filterWithKey (\k _ -> k `elem` chosenAgents) (Types.agents deploySpec)
        }

-- TODO: use prettyprinter
activate :: Env.Env -> Bool -> ByteString -> Deploy -> IO ()
activate Env.Env {cachixoptions, clientenv} deployAsync agentToken payload = do
  deployResponse <-
    escalate <=< (`runClientM` clientenv) $
      API.V2.activate deployClientV2 (Token agentToken) payload

  let agents = HM.toList (DeployResponse.agents deployResponse)

  Text.putStr (renderOverview agents)

  -- Skip streaming the logs when run with the --async flag
  when deployAsync exitSuccess

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

      deployment <- Async.withAsync (printLogsToTerminal options agentName) $ \logThread -> do
        deployment <- pollDeploymentStatus clientenv (Token agentToken) deploymentID

        -- Wait for all the logs to arrive
        let status = Deployment.status deployment
        when (status == Deployment.Failed || status == Deployment.Succeeded) $
          void (Async.waitCatch logThread)

        pure deployment

      pure (agentName, deployment)

pollDeploymentStatus :: ClientEnv -> Token -> UUID -> IO Deployment.Deployment
pollDeploymentStatus clientEnv token deploymentID = loop
  where
    loop = do
      deployment <-
        Retry.retryHttp $
          escalate <=< (`runClientM` clientEnv) $
            API.V1.getDeployment deployClientV1 token deploymentID

      case Deployment.status deployment of
        Deployment.Cancelled -> pure deployment
        Deployment.Failed -> pure deployment
        Deployment.Succeeded -> pure deployment
        _ -> do
          threadDelay (2 * 1000 * 1000)
          loop

printLogsToTerminal :: WebSocket.Options -> Text -> IO ()
printLogsToTerminal options agentName =
  WebSocket.runClientWith options WS.defaultConnectionOptions $ \connection ->
    fix $ \loop -> do
      message <- WS.receiveData connection
      case Aeson.eitherDecodeStrict' message of
        Left error -> do
          Text.putStrLn $ "Error parsing the log message: " <> show error
          loop
        Right msg -> do
          putStrLn $ unwords [inBrackets agentName, WSS.line msg]
          unless (isDeploymentDone (WSS.line msg)) loop
  where
    -- Parse each log line looking for the success/failure messages.
    -- TODO: figure out a way to avoid this. How can we tell when the log is done?
    isDeploymentDone :: Text -> Bool
    isDeploymentDone = isRight . Parse.parse logEndMessageParser ""

    logEndMessageParser :: Parse.Parsec Void Text Text
    logEndMessageParser =
      Parse.string "Successfully activated the deployment"
        <|> Parse.string "Failed to activate the deployment"

renderOverview :: [(Text, DeployResponse.Details)] -> Text
renderOverview agents =
  Text.intercalate "\n" $
    "Deploying agents:"
      : [ inBrackets agentName <> " " <> DeployResponse.url details
        | (agentName, details) <- agents
        ]

renderSummary :: [(Text, Deployment.Deployment)] -> Text
renderSummary results =
  Text.intercalate "\n" $
    "Deployment summary:"
      : [ inBrackets agentName <> " " <> renderStatus (Deployment.status deployment)
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
