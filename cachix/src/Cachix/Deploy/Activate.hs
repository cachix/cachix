{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Activate where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.InstallationMode as InstallationMode
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Deploy.Websocket as CachixWebsocket
import qualified Cachix.Types.BinaryCache as BinaryCache
import Cachix.Types.Permission (Permission (..))
import qualified Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Process as Conduit
import qualified Data.Vector as Vector
import Protolude hiding (log, toS)
import Protolude.Conv (toS)
import Servant.Auth.Client (Token (..))
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Prelude (String, userError)

activate ::
  -- | Logging context
  Conduit.ConduitT ByteString Void IO () ->
  -- | Profile name
  Text ->
  -- | Deployment details
  WSS.DeploymentDetails ->
  -- | Binary cache args
  [String] ->
  IO ()
activate logStream profileName deploymentDetails cacheArgs = do
  -- Download the store path from the binary cache
  -- TODO: add GC root so it's preserved for the next command
  shellOut "nix-store" (["-r", toS storePath] <> cacheArgs)

  (profilePath, activationScripts) <- getActivationScript storePath profileName

  -- Set the new profile
  -- TODO: document what happens if the wrong user is used for the agent
  shellOut "nix-env" ["-p", toS profilePath, "--set", toS storePath]

  -- Activate the configuration
  -- TODO: Check with Domen whether we can exit early here
  mapM_ (uncurry shellOut) activationScripts
  where
    storePath = WSS.storePath deploymentDetails

    shellOut :: FilePath -> [String] -> IO ()
    shellOut cmd args = shellOutWithExitCode cmd args >>= handleError
      where
        handleError (ExitFailure code) = ioError $ userError (show code)
        handleError ExitSuccess = pure ()

    shellOutWithExitCode :: FilePath -> [String] -> IO ExitCode
    shellOutWithExitCode cmd args = do
      log $ "$ " <> toS cmd <> " " <> toS (unwords $ fmap toS args)
      (exitCode, _, _) <- Conduit.sourceProcessWithStreams (proc cmd args) Conduit.sinkNull logStream logStream
      pure exitCode

    -- Move to Main.hs
    log :: ByteString -> IO ()
    log msg = Conduit.connect (Conduit.yieldMany ["\n" <> msg <> "\n"]) logStream

    -- TODO: home-manager
    -- TODO: move out of where clause
    getActivationScript :: Text -> Text -> IO (FilePath, [Command])
    getActivationScript storePath profile = do
      isNixOS <- doesPathExist $ toS storePath </> "nixos-version"
      isNixDarwin <- doesPathExist $ toS storePath </> "darwin-version"
      user <- InstallationMode.getUser
      (systemProfile, cmds) <- case (isNixOS, isNixDarwin) of
        (True, _) -> return ("system", [(toS storePath </> "bin/switch-to-configuration", ["switch"])])
        (_, True) ->
          -- https://github.com/LnL7/nix-darwin/blob/master/pkgs/nix-tools/darwin-rebuild.sh
          return
            ( "system-profiles/system",
              [ ("mkdir", ["-p", "-m", "0755", "/nix/var/nix/profiles/system-profiles"]),
                (toS storePath </> "activate-user", []),
                (toS storePath </> "activate", [])
              ]
            )
        (_, _) -> return ("system", [])
      return ("/nix/var/nix/profiles" </> if profile == "" then systemProfile else toS profile, cmds)

type Command = (String, [String])

-- TODO: send errors as well
-- TODO: fix either/maybe types
getClosureSize :: [String] -> Text -> IO (Either Text (Maybe Int64))
getClosureSize cacheArgs storePath = do
  (exitCode, pathInfoJSON, nixError) <- readProcessWithExitCode "nix" (cacheArgs <> ["--extra-experimental-features", "nix-command", "path-info", "-S", "--json", toS storePath]) ""
  case exitCode of
    ExitFailure _ -> pure $ Left (toS nixError)
    ExitSuccess -> pure $ Right $ Aeson.decode (toS pathInfoJSON) >>= extractClosureSize

extractClosureSize :: Aeson.Value -> Maybe Int64
extractClosureSize (Aeson.Array vector) = case Vector.toList vector of
  [Aeson.Object obj] -> case HM.lookup "closureSize" obj of
    Just (Aeson.Number num) -> Just $ floor num
    _ -> Nothing
  _ -> Nothing
extractClosureSize _ = Nothing

domain :: Text -> WSS.Cache -> Text
domain host cache = toS (WSS.cacheName cache) <> "." <> toS host

-- TODO: get uri scheme
uri :: Text -> WSS.Cache -> Text
uri host cache = "https://" <> domain host cache

-- TODO: don't create tmpfile for public caches
withCacheArgs :: Text -> WSS.AgentInformation -> ByteString -> ([String] -> IO a) -> IO a
withCacheArgs host agentInfo agentToken m =
  withSystemTempDirectory "netrc" $ \dir -> do
    let filepath = dir </> "netrc"
    args <- case WSS.cache agentInfo of
      Just cache -> do
        -- TODO: ugh
        let bc =
              BinaryCache.BinaryCache
                { BinaryCache.name = "",
                  BinaryCache.uri = toS (uri host cache),
                  BinaryCache.publicSigningKeys = [],
                  BinaryCache.isPublic = WSS.isPublic cache,
                  BinaryCache.githubUsername = "",
                  BinaryCache.permission = Read
                }
        NetRc.add (Token agentToken) [bc] filepath
        return $ cachesArgs <> ["--option", "netrc-file", filepath]
      Nothing ->
        return cachesArgs
    m args
  where
    cachesArgs :: [String]
    cachesArgs = case WSS.cache agentInfo of
      Just cache ->
        let officialCache = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            substituters = ["--option", "extra-substituters", toS (uri host cache)]
            noNegativeCaching = ["--option", "narinfo-cache-negative-ttl", "0"]
            sigs = ["--option", "trusted-public-keys", officialCache <> " " <> toS (domain host cache) <> "-1:" <> toS (WSS.publicKey cache)]
         in substituters ++ sigs ++ noNegativeCaching
      Nothing -> []
