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
            (progDesc "Deploy a new configuration to agents using CACHIX_ACTIVATE_TOKEN."),
        command "agent" $
          info
            (helper <*> agent)
            (progDesc "Run an agent in foreground using CACHIX_AGENT_TOKEN.")
      ]
  where
    activate = Activate <$> parserActivateOptions
    agent = Agent <$> parserAgentOptions

data AgentOptions = AgentOptions
  { name :: Text,
    profile :: Maybe Text
  }
  deriving (Show)

data ActivateOptions = ActivateOptions
  { payloadPath :: FilePath
  }
  deriving (Show)

parserAgentOptions :: Parser AgentOptions
parserAgentOptions =
  AgentOptions
    <$> strArgument
      ( metavar "AGENT-NAME"
          <> help "Unique identifier (usually hostname)."
      )
    <*> optional
      ( strArgument
          ( metavar "NIX-PROFILE"
              <> help "Nix profile to manage. Defaults to 'system' on NixOS and 'system-profiles/system' on nix-darwin."
          )
      )

parserActivateOptions :: Parser ActivateOptions
parserActivateOptions =
  ActivateOptions
    <$> strArgument
      ( metavar "DEPLOY-SPEC.JSON"
          <> help "https://docs.cachix.org/deploy/reference.html#deploy-json"
      )
