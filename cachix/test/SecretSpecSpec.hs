module SecretSpecSpec (spec) where

import Cachix.Client.Command.Config (secretspecCacheMetadata)
import Cachix.Client.Command.Push (getConfigSigningKey)
import Cachix.Client.Config qualified as Config
import Cachix.Client.SecretSpec qualified as SecretSpec
import Cachix.Client.URI qualified as URI
import Data.Text qualified as T
import Protolude hiding (toS)
import Servant.Auth.Client (Token (..))
import System.Directory (Permissions (executable), createDirectoryIfMissing, doesFileExist, getPermissions, setPermissions)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "secretspec cache metadata" $ do
    it "retains one empty cache entry while removing legacy signing keys" $ do
      let caches =
            [ Config.BinaryCacheConfig "other" "other-key",
              Config.BinaryCacheConfig "example" "old-key",
              Config.BinaryCacheConfig "example" "older-key"
            ]
          result = secretspecCacheMetadata "example" caches
      map (\cache -> (Config.name cache, Config.secretKey cache)) result
        `shouldBe` [("other", "other-key"), ("example", "")]

    it "does not let empty metadata shadow secretspec key lookup" $ do
      let cfg =
            Config.Config
              (Token "auth-token")
              URI.defaultCachixURI
              [Config.BinaryCacheConfig "example" ""]
      getConfigSigningKey cfg "example" `shouldBe` Nothing

  describe "configured signing keys" $ do
    it "prefers the newest nonempty key for the requested cache" $ do
      let cfg =
            Config.Config
              (Token "auth-token")
              URI.defaultCachixURI
              [ Config.BinaryCacheConfig "example" "old-key",
                Config.BinaryCacheConfig "example" "new-key",
                Config.BinaryCacheConfig "example" "",
                Config.BinaryCacheConfig "other" "other-key"
              ]
      getConfigSigningKey cfg "example" `shouldBe` Just "new-key"
      getConfigSigningKey cfg "missing" `shouldBe` Nothing

  when SecretSpec.supported $
    describe "secretspec integration" $ do
      it "detects configuration without invoking the CLI" $
        withFakeSecretspec $ \configPath argsPath _ -> do
          SecretSpec.isConfigured `shouldReturn` False
          withSetting "SECRETSPEC_PROVIDER" (Just "keyring") $
            SecretSpec.isConfigured `shouldReturn` True
          createDirectoryIfMissing True (takeDirectory configPath)
          writeFile configPath "[defaults]\nprovider = \"keyring\"\n"
          SecretSpec.isConfigured `shouldReturn` True
          doesFileExist argsPath `shouldReturn` False

      it "passes supplied secret values through stdin instead of argv" $
        withFakeSecretspec $ \_ argsPath stdinPath -> do
          let secret = "private-value-never-in-argv"
          SecretSpec.setAuthToken (Just secret)
          args <- readFile argsPath
          pipedValue <- readFile stdinPath
          args `shouldSatisfy` not . T.isInfixOf secret
          pipedValue `shouldBe` secret

      it "resolves the auth token from a real dotenv provider" $
        withSystemTempDirectory "cachix-secretspec-dotenv" $ \dir -> do
          let providerPath = dir </> "secrets.env"
              provider = "dotenv://" <> providerPath
              secret = "private-value-from-dotenv"
          writeFile providerPath ("CACHIX_AUTH_TOKEN=" <> secret <> "\n")
          withSetting "XDG_STATE_HOME" (Just (dir </> "state")) $
            withSetting "SECRETSPEC_PROVIDER" (Just provider) $
              SecretSpec.getAuthToken `shouldReturn` Just secret

      it "uses a fallback reason when SECRETSPEC_REASON is blank" $
        withFakeSecretspec $ \_ argsPath _ ->
          withSetting "SECRETSPEC_REASON" (Just "   ") $ do
            SecretSpec.setAuthToken (Just "private-value")
            args <- readFile argsPath
            args `shouldSatisfy` T.isInfixOf "cachix credential store"

      it "preserves a non-empty SECRETSPEC_REASON" $
        withFakeSecretspec $ \_ argsPath _ ->
          withSetting "SECRETSPEC_REASON" (Just "operator supplied reason") $ do
            SecretSpec.setAuthToken (Just "private-value")
            args <- readFile argsPath
            args `shouldSatisfy` not . T.isInfixOf "cachix credential store"

withFakeSecretspec :: (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withFakeSecretspec action =
  withSystemTempDirectory "cachix-fake-secretspec" $ \dir -> do
    oldPath <- lookupEnv "PATH"
    let binaryPath = dir </> "secretspec"
        configHome = dir </> "config"
        configPath = configHome </> "secretspec" </> "config.toml"
        argsPath = dir </> "args"
        stdinPath = dir </> "stdin"
        path = dir <> maybe "" (":" <>) oldPath
    writeFile binaryPath fakeSecretspec
    permissions <- getPermissions binaryPath
    setPermissions binaryPath permissions {executable = True}
    withSetting "PATH" (Just path) $
      withSetting "XDG_CONFIG_HOME" (Just configHome) $
        withSetting "SECRETSPEC_PROVIDER" Nothing $
          withSetting "FAKE_ARGS_FILE" (Just argsPath) $
            withSetting "FAKE_STDIN_FILE" (Just stdinPath) $
              action configPath argsPath stdinPath

withSetting :: [Char] -> Maybe [Char] -> IO a -> IO a
withSetting name value action = bracket acquire restore (const action)
  where
    acquire = do
      oldValue <- lookupEnv name
      set value
      return oldValue
    restore = set
    set (Just newValue) = setEnv name newValue
    set Nothing = unsetEnv name

fakeSecretspec :: Text
fakeSecretspec =
  "#!/bin/sh\n\
  \printf '%s\\n' \"$@\" > \"$FAKE_ARGS_FILE\"\n\
  \cat > \"$FAKE_STDIN_FILE\"\n"
