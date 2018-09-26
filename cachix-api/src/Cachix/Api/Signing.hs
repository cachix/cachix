module Cachix.Api.Signing
  ( fingerprint
  , passthroughSizeSink
  , passthroughHashSink
  ) where

import           Crypto.Hash
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteArray                as BA
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16        as B16
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

passthroughHashSink :: MonadIO m => IORef Text -> ConduitT ByteString ByteString m ()
passthroughHashSink ioref = passthroughSink hashSink (liftIO . writeIORef ioref . transf)
  where
    -- TODO: use cryptonite B16 to get rid of extra dep and simplify
    transf = toS . B16.encode . BS.pack . BA.unpack . hashFinalize
