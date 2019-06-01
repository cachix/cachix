module Cachix.Client.Commands.Export where

import Protolude
import Cachix.Client.Commands.Export.OptionsParser
import qualified Cachix.Client.Export as Export
import Cachix.Client.Env
import qualified Cachix.Client.Exception as Exception
import Cachix.Api.Error
import System.IO (hClose)
import qualified Data.Aeson
import qualified Data.Text as T
import qualified Data.Validation as Validation
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BB

run :: Env -> Arguments -> IO ()
run env args = do

  cfg <- escalate $ maybeToEither (Exception.NoConfig "I can not export from an\
    \ empty configuration. Please set up a cache or use the --config option if\
    \ you've already done so.") $ config env

  encodings1V <- Export.exportPushSecrets env cfg (pushSecrets args)
  encodings2V <- Export.exportPullSecrets env cfg (pullSecrets args)

  encodings <- escalateAs (FatalError . T.concat . map (<> "\n") . toList)
             $ Validation.toEither (
                 (++) <$> encodings1V <*> encodings2V
               )

  let
    open = case outputFile args of
        Stdout -> pure stdout
        Out f -> openFile f WriteMode
        Append f -> openFile f AppendMode

    withOutputHandle = bracket open hClose

  withOutputHandle $ \h ->
    forM_ encodings $ \encoding ->
      BL.hPutStr h (BB.toLazyByteString
                      (Data.Aeson.fromEncoding encoding <> "\n"))
