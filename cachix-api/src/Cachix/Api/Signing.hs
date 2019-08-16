module Cachix.Api.Signing
  ( fingerprint
  , passthroughSizeSink
  , passthroughHashSinkB16
  , passthroughHashSink
  ) where

import           Crypto.Hash
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.ByteArray as BA
import           Data.ByteArray.Encoding (convertToBase, Base(..))
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Combinators      as CC
import           Data.IORef
import           Data.String.Conv (toS)
import qualified Data.Text as T
import           Data.Text (Text)


-- perl/lib/Nix/Manifest.pm:fingerprintPath
-- NB: references must be sorted
fingerprint :: Text -> Text -> Integer -> [Text] -> ByteString
fingerprint storePath narHash narSize references = toS $ T.intercalate ";"
  ["1", storePath, narHash, T.pack (show narSize), T.intercalate "," references]

-- Useful sinks for streaming nars

sizeSink :: MonadIO m => ConduitT ByteString o m Integer
sizeSink = CC.foldM (\p n -> return (p + fromIntegral (BS.length n))) 0

hashSink :: MonadIO m => ConduitT ByteString o m (Context SHA256)
hashSink = CC.foldM (\p n -> return (hashUpdate p n)) hashInit

passthroughSizeSink :: MonadIO m => IORef Integer -> ConduitT ByteString ByteString m ()
passthroughSizeSink ioref = passthroughSink sizeSink (liftIO . writeIORef ioref)

passthroughHashSinkBase :: MonadIO m => (Digest SHA256 -> ByteString) -> IORef ByteString -> ConduitT ByteString ByteString m ()
passthroughHashSinkBase f ioref = passthroughSink hashSink (liftIO . writeIORef ioref . f . hashFinalize)

passthroughHashSink :: MonadIO m => IORef ByteString -> ConduitT ByteString ByteString m ()
passthroughHashSink = passthroughHashSinkBase BA.convert

passthroughHashSinkB16 :: MonadIO m => IORef ByteString -> ConduitT ByteString ByteString m ()
passthroughHashSinkB16 = passthroughHashSinkBase (convertToBase Base16)
