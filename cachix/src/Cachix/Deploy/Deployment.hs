module Cachix.Deploy.Deployment where

import Cachix.API.WebSocketSubprotocol qualified as WSS
import Cachix.Client.URI as Cachix
import Cachix.Deploy.Log qualified as Log
import Data.Aeson qualified as Aeson
import Protolude

-- | Everything required for the standalone deployment binary to complete a
-- deployment.
data Deployment = Deployment
  { agentName :: Text,
    agentToken :: Text,
    profileName :: Text,
    agentInformation :: WSS.AgentInformation,
    deploymentDetails :: WSS.DeploymentDetails,
    host :: Cachix.URI,
    logOptions :: Log.Options
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
