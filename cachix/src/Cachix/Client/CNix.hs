module Cachix.Client.CNix where

import qualified Data.Text as T
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
import System.Console.Pretty (Color (..), color)

predicateInvalidPath :: SomeException -> Maybe Text
predicateInvalidPath e
  | Just (C.CppStdException msg) <- fromException e =
    if "nix::InvalidPath" `T.isSuffixOf` toS msg
      then Just $ toS msg
      else Nothing
predicateInvalidPath _ = Nothing

handleInvalidPath :: IO (Maybe a) -> IO (Maybe a)
handleInvalidPath = handleJust predicateInvalidPath handle_
  where
    handle_ :: Text -> IO (Maybe a)
    handle_ msg = do
      hPutStrLn stderr $ color Yellow $ "Warning: " <> msg
      return Nothing
