{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Cachix.Client.Command.Import where

import Amazonka qualified
import Amazonka.Data.Body (ResponseBody (..))
import Amazonka.Data.Text qualified
import Amazonka.S3 qualified
import Amazonka.S3.GetObject (getObjectResponse_body)
import Amazonka.S3.ListObjectsV2 (listObjectsV2Response_contents)
import Amazonka.S3.Types.Object (object_key)
import Cachix.API.Error
import Cachix.Client.Command.Push
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push
import Cachix.Client.Servant
import Cachix.Client.URI (URI)
import Cachix.Client.URI qualified as URI
import Conduit
import Control.Retry (defaultRetryStatus)
import Data.Attoparsec.Text qualified
import Data.Conduit.Combinators qualified as C
import Data.Conduit.ConcurrentMap (concurrentMapM)
import Data.Conduit.List qualified as CL
import Data.Generics.Labels ()
import Data.Text qualified as T
import Hercules.CNix.Store (parseStorePath)
import Lens.Micro
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status404)
import Nix.NarInfo qualified as NarInfo
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API (NoContent (..))
import URI.ByteString qualified as UBS

discoverAwsEnv :: Maybe ByteString -> Maybe ByteString -> IO Amazonka.Env
discoverAwsEnv maybeEndpoint maybeRegion = do
  s3Endpoint <-
    case maybeEndpoint of
      Nothing -> pure Amazonka.S3.defaultService
      Just url -> do
        req <- HTTP.parseRequest (toS url)
        pure $
          Amazonka.S3.defaultService
            & Amazonka.setEndpoint (HTTP.secure req) (HTTP.host req) (HTTP.port req)
            -- Don't overwrite requests into the virtual-hosted style i.e. <bucket>.<host>.
            -- This would break IP-based endpoints, like our test endpoint.
            & #s3AddressingStyle
              .~ Amazonka.S3AddressingStylePath

  region <-
    traverse (escalateAs (FatalError . toS) . Amazonka.Data.Text.fromText . toS) maybeRegion

  -- Create a service client with the custom endpoint
  Amazonka.newEnv Amazonka.discover
    <&> Amazonka.configureService s3Endpoint
      . maybe identity (#region .~) region

import' :: Env -> PushOptions -> Text -> URI -> IO ()
import' env pushOptions name s3uri = do
  awsEnv <- discoverAwsEnv (URI.getQueryParam s3uri "endpoint") (URI.getQueryParam s3uri "region")
  putErrText $ "Importing narinfos/nars using " <> show (numJobs pushOptions) <> " workers from " <> URI.serialize s3uri <> " to " <> name
  putErrText ""
  Amazonka.runResourceT $
    runConduit $
      Amazonka.paginate awsEnv (Amazonka.S3.newListObjectsV2 bucketName)
        .| CL.mapMaybe (^. listObjectsV2Response_contents)
        .| CL.concat
        .| CL.map (^. object_key)
        .| CL.filter (T.isSuffixOf ".narinfo" . Amazonka.Data.Text.toText)
        .| concurrentMapM (numJobs pushOptions) (numJobs pushOptions * 2) (uploadNarinfo awsEnv)
        .| CL.sinkNull
  putErrText "All done."
  where
    bucketName :: Amazonka.S3.BucketName
    bucketName = Amazonka.S3.BucketName bucketNameText
    bucketNameText = toS $ UBS.hostBS $ URI.getHostname s3uri

    getObject ::
      (MonadResource m) =>
      Amazonka.Env ->
      Amazonka.S3.ObjectKey ->
      m (ConduitT () ByteString (ResourceT IO) ())
    getObject awsEnv key = do
      rs <- Amazonka.send awsEnv (Amazonka.S3.newGetObject bucketName key)
      return $ body $ rs ^. getObjectResponse_body

    fileHashParse :: Text -> IO Text
    fileHashParse s
      | "sha256:" `T.isPrefixOf` s = return $ T.drop 7 s
      | otherwise = throwM $ ImportUnsupportedHash $ "file hash " <> s <> " is unsupported. Leave us feedback at https://github.com/cachix/cachix/issues/601"

    uploadNarinfo :: Amazonka.Env -> Amazonka.S3.ObjectKey -> ResourceT IO ()
    uploadNarinfo awsEnv entry = liftIO $ do
      let storeHash = T.dropEnd 8 $ Amazonka.Data.Text.toText entry

      -- get narinfo
      narinfoText <- runConduitRes $ do
        narinfoStream <- getObject awsEnv entry
        narinfoStream .| C.decodeUtf8 .| C.fold

      -- parse narinfo
      case Data.Attoparsec.Text.parseOnly NarInfo.parseNarInfo (toS narinfoText) of
        Left e -> hPutStr stderr $ "error while parsing " <> storeHash <> ": " <> show e
        Right parsedNarInfo -> do
          -- we support only sha256: for now
          narInfo <- do
            fileHash <- fileHashParse $ NarInfo.fileHash parsedNarInfo
            return $ parsedNarInfo {NarInfo.fileHash = fileHash}

          -- stream nar and narinfo
          liftIO $ withPushParams env pushOptions name $ \pushParams -> do
            narinfoResponse <- liftIO $ narinfoExists pushParams (toS storeHash)
            let storePathText = NarInfo.storePath narInfo
                store = pushParamsStore pushParams
            storePath <- liftIO $ parseStorePath store (toS storePathText)
            let strategy = pushParamsStrategy pushParams storePath

            case narinfoResponse of
              Right NoContent -> onAlreadyPresent strategy
              Left err
                | isErr err status404 -> runResourceT $ do
                    pathInfo <- newPathInfoFromNarInfo narInfo
                    let fileSize = fromInteger $ NarInfo.fileSize narInfo

                    case readMaybe (NarInfo.compression narInfo) of
                      Nothing -> putErrText $ "Unsupported compression method: " <> NarInfo.compression narInfo
                      Just compressionMethod -> do
                        liftIO $ onAttempt strategy defaultRetryStatus fileSize

                        narStream <- getObject awsEnv $ Amazonka.S3.ObjectKey $ NarInfo.url narInfo

                        res <-
                          runConduit $
                            narStream
                              .| streamCopy pushParams storePath fileSize defaultRetryStatus compressionMethod

                        case res of
                          Left uploadErr ->
                            liftIO $ onError strategy uploadErr
                          Right (uploadResult, uploadNarDetails) -> do
                            -- TODO: Check that the file size matches?
                            -- Copy over details about the NAR from the narinfo.
                            let newNarDetails = uploadNarDetails {undNarSize = NarInfo.narSize narInfo, undNarHash = NarInfo.narHash narInfo}

                            nic <- newNarInfoCreate pushParams storePath pathInfo newNarDetails
                            completeNarUpload pushParams uploadResult nic

                            liftIO $ onDone strategy
                | otherwise -> putErrText $ show err
