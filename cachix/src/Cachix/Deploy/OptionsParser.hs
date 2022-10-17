module Cachix.Deploy.OptionsParser where

import Options.Applicative
import Protolude

data DeployCommand
  = Activate ActivateOptions
  | Agent AgentOptions
  deriving (Show)

parser :: Parser DeployCommand
parser =
  subparser $
    mconcat
      [ command "activate" $
          info
            (helper <*> activate)
            (progDesc "Deploy a new configuration to agents using CACHIX_ACTIVATE_TOKEN. See https://docs.cachix.org/deploy/deploying-to-agents"),
        command "agent" $
          info
            (helper <*> agent)
            (progDesc "Run an agent in foreground using CACHIX_AGENT_TOKEN. See https://docs.cachix.org/deploy/running-an-agent/")
      ]
  where
    activate = Activate <$> parserActivateOptions
    agent = Agent <$> parserAgentOptions

data AgentOptions = AgentOptions
  { name :: Text,
    profile :: Maybe Text,
    bootstrap :: Bool
  }
  deriving (Show)

data ActivateOptions = ActivateOptions
  { payloadPath :: FilePath,
    agents :: [Text]
  }
  deriving (Show)

parserAgentOptions :: Parser AgentOptions
parserAgentOptions =
  AgentOptions
    <$> strArgument
      ( metavar "AGENT-NAME"
          <> help "Unique agent identifier (usually hostname)."
      )
    <*> optional
      ( strArgument
          ( metavar "NIX-PROFILE"
              <> help "Nix profile to manage. Defaults to 'system' on NixOS, 'system-profiles/system' (nix-darwin) on macOS, and 'home-manager' for Home Manager."
          )
      )
    <*> switch (long "bootstrap" <> help "Exit once the system agent takes over.")

parserActivateOptions :: Parser ActivateOptions
parserActivateOptions =
  ActivateOptions
    <$> strArgument
      ( metavar "DEPLOY-SPEC.JSON"
          <> help "https://docs.cachix.org/deploy/reference.html#deploy-json"
      )
    <*> many
      ( strOption
          ( long "agent"
              <> short 'a'
              <> metavar "AGENT-NAME"
              <> help "Deploy only specific agent(s)."
          )
      )
