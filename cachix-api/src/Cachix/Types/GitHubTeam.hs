module Cachix.Types.GitHubTeam
  ( GitHubTeam (..)
    )
where

import Data.Aeson
  ( FromJSON,
    ToJSON
    )
import Data.Swagger
import Protolude

data GitHubTeam
  = GitHubTeam
      { id :: Int,
        name :: Text
        }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
