{-# LANGUAGE QuasiQuotes #-}

module NixConfSpec where

import Cachix.Client.NixConf as NixConf
import Cachix.Types.BinaryCache (BinaryCache (..), CompressionMethod (..))
import Cachix.Types.Permission (Permission (..))
import Data.String.Here
import Protolude
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Hspec

property :: Text -> Expectation
property x = NixConf.render <$> parse x `shouldBe` Right x

bc :: BinaryCache
bc =
  BinaryCache
    { name = "name",
      uri = "https://name.cachix.org",
      isPublic = True,
      permission = Admin,
      publicSigningKeys = ["pub"],
      githubUsername = "foobar",
      preferredCompressionMethod = ZSTD
    }

spec :: Spec
spec = do
  describe "render . parse" $ do
    it "handles single value substituters" $
      property "substituters = a\n"
    it "handles multi value substituters" $
      property "substituters = a b c\n"
    it "handles all known keys" $
      property "substituters = a b c\ntrusted-users = him me\ntrusted-public-keys = a\n"
    it "handles includes" $
      property "include /etc/nix/nix.conf\n!include /etc/nix/nix.conf\n"
    it "random content" $
      property "blabla = foobar\nfoo = bar\n"

  describe "add" $ do
    it "merges binary caches from both files" $
      let globalConf =
            NixConf
              [ Substituters ["bc1"],
                TrustedPublicKeys ["pub1"]
              ]
          localConf =
            NixConf
              [ Substituters ["bc2"],
                TrustedPublicKeys ["pub2"]
              ]
          result =
            NixConf
              [ Substituters [defaultPublicURI, "bc1", "bc2", "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "pub1", "pub2", "pub"]
              ]
       in add bc [globalConf, localConf] localConf `shouldBe` result

    it "is noop if binary cache exists in one file" $
      let globalConf =
            NixConf
              [ Substituters [defaultPublicURI, "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "pub"]
              ]
          localConf = NixConf []
          result =
            NixConf
              [ Substituters [defaultPublicURI, "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "pub"]
              ]
       in add bc [globalConf, localConf] localConf `shouldBe` result

    it "preserves other nixconf entries" $
      let globalConf =
            NixConf
              [ Substituters ["http"],
                TrustedPublicKeys ["pub1"],
                TrustedUsers ["user"],
                Other "foo"
              ]
          localConf =
            NixConf
              [ TrustedUsers ["user2"],
                Other "bar"
              ]
          result =
            NixConf
              [ TrustedUsers ["user2"],
                Other "bar",
                Substituters [defaultPublicURI, "http", "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "pub1", "pub"]
              ]
       in add bc [globalConf, localConf] localConf `shouldBe` result

    it "removed duplicates" $
      let globalConf =
            NixConf
              [ Substituters ["bc1", "bc1"],
                TrustedPublicKeys ["pub1", "pub1"]
              ]
          localConf =
            NixConf
              [ Substituters ["bc2", "bc2"],
                TrustedPublicKeys ["pub2", "pub2"]
              ]
          result =
            NixConf
              [ Substituters [defaultPublicURI, "bc1", "bc2", "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "pub1", "pub2", "pub"]
              ]
       in add bc [globalConf, localConf] localConf `shouldBe` result

    it "adds binary cache and defaults if no existing entries exist" $
      let globalConf = NixConf []
          localConf = NixConf []
          result =
            NixConf
              [ Substituters [defaultPublicURI, "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "pub"]
              ]
       in add bc [globalConf, localConf] localConf `shouldBe` result

  describe "remove" $ do
    it "removes a binary cache" $
      let localConf =
            NixConf
              [ Substituters [defaultPublicURI, "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "name.cachix.org-1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa="]
              ]
          result =
            NixConf
              [ Substituters [defaultPublicURI],
                TrustedPublicKeys [defaultSigningKey]
              ]
       in remove "https://cachix.org" "name" [localConf] localConf `shouldBe` (result, True)

    it "removes nothing if the binary cache is missing" $
      let globalConf =
            NixConf
              [ Substituters [defaultPublicURI],
                TrustedPublicKeys [defaultSigningKey]
              ]
          localConf = globalConf
          result = localConf
       in remove "https://cachix.org" "name" [globalConf, localConf] localConf `shouldBe` (result, False)

  describe "parse" $ do
    it "parses substituters" $
      parse "substituters = a\n"
        `shouldBe` Right (NixConf [Substituters ["a"]])

    it "parses long key" $
      parse "binary-caches-parallel-connections = 40\n"
        `shouldBe` Right (NixConf [Other "binary-caches-parallel-connections = 40"])

    it "parses substituters with multiple values" $
      parse "substituters = a b c\n"
        `shouldBe` Right (NixConf [Substituters ["a", "b", "c"]])

    it "parses equal sign after the first key as literal" $
      parse "substituters = a b c= d\n"
        `shouldBe` Right (NixConf [Substituters ["a", "b", "c=", "d"]])

    it "parses with missing endline" $
      parse "allowed-users = *"
        `shouldBe` Right (NixConf [Other "allowed-users = *"])

    it "parses include" $
      parse "include /etc/nix/nix.conf\n"
        `shouldBe` Right (NixConf [Include (RequiredInclude "/etc/nix/nix.conf")])

    it "parses !include" $
      parse "!include /etc/nix/nix.conf\n"
        `shouldBe` Right (NixConf [Include (OptionalInclude "/etc/nix/nix.conf")])

    it "parses a complex example" $
      parse realExample
        `shouldBe` Right parsedRealExample

  describe "write . read" $ do
    it "is idempotent" $ do
      withTempDirectory "/tmp" "nixconf" $ \temp -> do
        let confPath = temp </> "nix.conf"
        let subConfPath = temp </> "sub.conf"
        let confContents = "include " <> toS subConfPath <> "\n"
        let parsedConfContents = NixConf [Include (RequiredInclude (toS subConfPath))]
        writeFile confPath confContents
        writeFile subConfPath realExample

        Just conf <- NixConf.read (Custom temp)
        conf `shouldBe` parsedConfContents
        NixConf.write (Custom temp) conf
        readFile confPath `shouldReturn` confContents

        NixConf.resolveIncludes confPath conf
          `shouldReturn` [ parsedConfContents,
                           parsedRealExample
                         ]

realExample :: Text
realExample =
  [hereLit|
substituters = a  b c
trusted-users = him me
trusted-public-keys  =  a
blabla =  asd
# comment


|]

parsedRealExample :: NixConf
parsedRealExample =
  NixConf
    [ Other "",
      Substituters ["a", "b", "c"],
      TrustedUsers ["him", "me"],
      TrustedPublicKeys ["a"],
      Other "blabla =  asd",
      Other "# comment",
      Other "",
      Other ""
    ]
