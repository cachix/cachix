module Cachix.Client.NixVersion
  ( getNixMode
  , parseNixMode
  , NixMode(..)
  ) where

import Protolude
import System.Process (readProcessWithExitCode)
import Data.Versions
import Data.Text as T


data NixMode
  = Nix20
  | Nix201
  | Nix1XX
  deriving (Show, Eq)

getNixMode :: IO (Either Text NixMode)
getNixMode = do
  (exitcode, out, err) <- readProcessWithExitCode "nix-env" ["--version"] mempty
  unless (err == "") $ putStrLn $ "nix-env stderr: " <> err
  return $ case exitcode of
    ExitFailure i -> Left $ "'nix-env --version' exited with " <> show i
    ExitSuccess -> parseNixMode $ toS out

parseNixMode :: Text -> Either Text NixMode
parseNixMode output =
  let
    verStr = T.drop 14 $ T.strip output
    err = "Couldn't parse 'nix-env --version' output: " <> output
  in case parseV verStr of
    Left _ -> Left err
    Right ver | verStr == "" -> Left err
              | ver < Ideal (SemVer 1 99 0 [] []) -> Right Nix1XX
              | ver < Ideal (SemVer 2 0 1 [] []) -> Right Nix20
              | otherwise -> Right Nix201
