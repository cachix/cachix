-- TODO: we may need to revisit this when the various flavours of Nix start to diverge
module Cachix.Client.NixVersion
  ( assertNixVersion,
    isSupportedNixVersion,
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

-- | The minimum supported Nix version. Kept as a General (two component)
-- version so plain "2.4" output compares equal to it; the cross constructor
-- Ord of the versions package orders SemVer shaped output like "2.4.0"
-- correctly against it as well.
minimalVersion :: Versioning
minimalVersion =
  General (version "2.4" & fromRight (panic "Couldn't parse minimalVersion"))

isSupportedNixVersion :: Versioning -> Bool
isSupportedNixVersion = (>= minimalVersion)

assertNixVersion :: IO (Either Text ())
assertNixVersion = do
  nixVersion <- fmap (>>= parseNixVersion) getRawNixVersion
  return $ case nixVersion of
    Left err -> Left err
    Right ver
      | not (isSupportedNixVersion ver) -> Left $ "Nix " <> prettyV minimalVersion <> " or newer is required. Please upgrade: https://nixos.org/nix/"
      | otherwise -> Right ()

getRawNixVersion :: IO (Either Text Text)
getRawNixVersion = do
  result <- try (readProcessWithExitCode "nix-env" ["--version"] mempty) :: IO (Either IOException (ExitCode, [Char], [Char]))
  case result of
    Left ioerr ->
      return $
        Left $
          "Couldn't run 'nix-env --version': "
            <> toS (displayException ioerr)
            <> "\nIs Nix installed and on the PATH? https://nixos.org/nix/"
    Right (exitcode, out, err) -> do
      unless (err == "") $ putStrLn $ "nix-env stderr: " <> err
      return $ case exitcode of
        ExitFailure i -> Left $ "'nix-env --version' exited with " <> Protolude.show i
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
