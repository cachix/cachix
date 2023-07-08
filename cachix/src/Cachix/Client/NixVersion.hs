module Cachix.Client.NixVersion
  ( assertNixVersion,
    parseNixVersion,
  )
where

import Data.Text as T
import Data.Versions
import Protolude
import System.Process (readProcessWithExitCode)

assertNixVersion :: IO (Either Text ())
assertNixVersion = do
  (exitcode, out, err) <- readProcessWithExitCode "nix-env" ["--version"] mempty
  unless (err == "") $ putStrLn $ "nix-env stderr: " <> err
  return $ case exitcode of
    ExitFailure i -> Left $ "'nix-env --version' exited with " <> show i
    ExitSuccess -> parseNixVersion $ toS out

parseNixVersion :: Text -> Either Text ()
parseNixVersion output =
  let verStr = T.drop 14 $ T.strip output
      err = "Couldn't parse 'nix-env --version' output: " <> output
   in case versioning verStr of
        Left _ -> Left err
        Right ver
          | verStr == "" -> Left err
          | ver < Ideal minimalVersion -> Left "Nix 2.0.2 or lower is not supported. Please upgrade: https://nixos.org/nix/"
          | otherwise -> Right ()

minimalVersion :: SemVer
minimalVersion =
  semver "2.0.1" & fromRight (panic "Couldn't parse minimalVersion")
