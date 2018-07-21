module Cachix.Client.NixVersion
  ( getNixVersion
  , parseNixVersion
  , NixVersion(..)
  ) where

import Protolude
import System.Process (readProcessWithExitCode)
import Data.Versions
import Data.Text as T


data NixVersion
  = Nix20
  | Nix201
  | Nix1XX
  deriving (Show, Eq)

getNixVersion :: IO (Either Text NixVersion)
getNixVersion = do
  (exitcode, out, err) <- readProcessWithExitCode "nix-env" ["--version"] mempty
  unless (err == "") $ putStrLn $ "nix-env stderr: " <> err
  return $ case exitcode of
    ExitFailure i -> Left $ "'nix-env --version' exited with " <> show i
    ExitSuccess -> parseNixVersion $ toS out

parseNixVersion :: Text -> Either Text NixVersion
parseNixVersion output =
  let
    verStr = T.drop 14 $ T.strip output
    err = "Couldn't parse 'nix-env --version' output: " <> output
  in case versioning verStr of
    Left _ -> Left err
    Right ver | verStr == "" -> Left err
              | ver < Ideal (SemVer 1 99 0 [] []) -> Right Nix1XX
              | ver < Ideal (SemVer 2 0 1 [] []) -> Right Nix20
              | otherwise -> Right Nix201
