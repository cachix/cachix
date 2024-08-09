-- TODO: we may need to revisit this when the various flavours of Nix start to diverge
module Cachix.Client.NixVersion
  ( assertNixVersion,
    parseNixVersion,
    minimalVersion,
  )
where

import Data.Either.Extra (mapLeft)
import Data.Text as T
import Data.Versions hiding (versioning')
import Protolude
import System.Process (readProcessWithExitCode)
import Text.Megaparsec (Parsec, anySingle, choice, eof, lookAhead, manyTill, parse)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char (digitChar)

minimalVersion :: SemVer
minimalVersion =
  semver "2.0.1" & fromRight (panic "Couldn't parse minimalVersion")

assertNixVersion :: IO (Either Text ())
assertNixVersion = do
  nixVersion <- fmap (>>= parseNixVersion) getRawNixVersion
  return $ case nixVersion of
    Left err -> Left err
    Right ver
      | ver < Ideal minimalVersion -> Left "Nix 2.0.2 or lower is not supported. Please upgrade: https://nixos.org/nix/"
      | otherwise -> Right ()

getRawNixVersion :: IO (Either Text Text)
getRawNixVersion = do
  (exitcode, out, err) <- readProcessWithExitCode "nix-env" ["--version"] mempty
  unless (err == "") $ putStrLn $ "nix-env stderr: " <> err
  return $ case exitcode of
    ExitFailure i -> Left $ "'nix-env --version' exited with " <> show i
    ExitSuccess -> Right (toS out)

parseNixVersion :: Text -> Either Text Versioning
parseNixVersion input =
  mapLeft fromParsingError $ parse nixVersionParser "Nix version" input
  where
    fromParsingError pe =
      unlines
        [ "Couldn't parse 'nix-env --version' output: " <> input,
          T.pack $ errorBundlePretty pe
        ]

-- | Parses a semver string out of the output of `nix-env --version` or `nix --version`.
nixVersionParser :: Parsec Void Text Versioning
nixVersionParser = do
  _ <- manyTill anySingle (lookAhead digitChar)
  v <- versioning'
  _ <- manyTill anySingle eof
  pure v

-- | Same as `versioning'`, but without the restriction of preceding eof.
versioning' :: Parsec Void Text Versioning
versioning' =
  choice
    [ P.try (fmap Ideal semver'),
      P.try (fmap General version'),
      fmap Complex mess'
    ]
