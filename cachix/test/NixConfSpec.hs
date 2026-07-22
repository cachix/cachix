{-# LANGUAGE QuasiQuotes #-}

module NixConfSpec where

import Cachix.Client.Exception (CachixException (CircularInclude))
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

-- | The !include directive cachix adds to the nix.conf to pull in its fragment.
cachixInclude :: NixConfLine
cachixInclude = Include (OptionalInclude "cachix.conf")

spec :: Spec
spec = do
  describe "render . parse" $ do
    it "handles single value substituters" $
      property "substituters = a\n"
    it "handles multi value substituters" $
      property "substituters = a b c\n"
    it "handles extra substituters" $
      property "extra-substituters = a b c\n"
    it "handles all known keys" $
      property "substituters = a b c\nextra-substituters = d\ntrusted-users = him me\ntrusted-public-keys = a\nextra-trusted-public-keys = b\n"
    it "handles includes" $
      property "include /etc/nix/nix.conf\n!include /etc/nix/nix.conf\n"
    it "random content" $
      property "blabla = foobar\nfoo = bar\n"

  describe "addCache" $ do
    it "writes the cache to the fragment and includes it from an empty nix.conf" $
      let result =
            ( NixConf [cachixInclude],
              NixConf
                [ ExtraSubstituters ["https://name.cachix.org"],
                  ExtraTrustedPublicKeys ["pub"]
                ]
            )
       in addCache bc (NixConf []) (NixConf []) `shouldBe` result

    it "accumulates caches in the fragment across runs" $
      let nixConf = NixConf [cachixInclude]
          fragment =
            NixConf
              [ ExtraSubstituters ["https://other.cachix.org"],
                ExtraTrustedPublicKeys ["other-key"]
              ]
          result =
            ( NixConf [cachixInclude],
              NixConf
                [ ExtraSubstituters ["https://other.cachix.org", "https://name.cachix.org"],
                  ExtraTrustedPublicKeys ["other-key", "pub"]
                ]
            )
       in addCache bc nixConf fragment `shouldBe` result

    it "migrates inline settings written by older versions into the fragment" $
      -- The Nix default the legacy line carried is restated in the fragment:
      -- the old line overrode substituters set at other config levels, so
      -- dropping the default could remove cache.nixos.org from the effective
      -- config on such setups.
      let nixConf =
            NixConf
              [ Substituters [defaultPublicURI, "https://other.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "other-key"]
              ]
          result =
            ( NixConf [cachixInclude],
              NixConf
                [ ExtraSubstituters [defaultPublicURI, "https://other.cachix.org", "https://name.cachix.org"],
                  ExtraTrustedPublicKeys [defaultSigningKey, "other-key", "pub"]
                ]
            )
       in addCache bc nixConf (NixConf []) `shouldBe` result

    it "leaves unrelated nix.conf settings in place" $
      -- Substituters/TrustedPublicKeys lines that don't carry the Nix default
      -- (defaultPublicURI/defaultSigningKey) an older cachix always force-wrote
      -- alongside its own entries aren't recognized as cachix's, so they must
      -- stay in nix.conf untouched; only the new cache goes into the fragment.
      let nixConf =
            NixConf
              [ TrustedUsers ["user2"],
                Other "bar",
                Substituters ["http"],
                TrustedPublicKeys ["pub1"]
              ]
          result =
            ( NixConf
                [ TrustedUsers ["user2"],
                  Other "bar",
                  Substituters ["http"],
                  TrustedPublicKeys ["pub1"],
                  cachixInclude
                ],
              NixConf
                [ ExtraSubstituters ["https://name.cachix.org"],
                  ExtraTrustedPublicKeys ["pub"]
                ]
            )
       in addCache bc nixConf (NixConf []) `shouldBe` result

    it "does not migrate a substituters line missing the default cachix always wrote alongside its own" $
      -- A line starting with defaultPublicURI is recognized as cachix's
      -- legacy write; one without it (even one that happens to mention a
      -- cachix cache URL, e.g. hand-copied by a user) is left alone.
      let nixConf = NixConf [Substituters ["https://other.cachix.org"]]
          result =
            ( NixConf [Substituters ["https://other.cachix.org"], cachixInclude],
              NixConf
                [ ExtraSubstituters ["https://name.cachix.org"],
                  ExtraTrustedPublicKeys ["pub"]
                ]
            )
       in addCache bc nixConf (NixConf []) `shouldBe` result

    it "does not migrate a substituters line where the default is not the first value" $
      -- Old cachix always wrote defaultPublicURI as the FIRST value, so a
      -- line merely containing it elsewhere is user-authored and stays put.
      let nixConf = NixConf [Substituters ["https://other.example", defaultPublicURI]]
          result =
            ( NixConf [Substituters ["https://other.example", defaultPublicURI], cachixInclude],
              NixConf
                [ ExtraSubstituters ["https://name.cachix.org"],
                  ExtraTrustedPublicKeys ["pub"]
                ]
            )
       in addCache bc nixConf (NixConf []) `shouldBe` result

    it "migrates a legacy line without sweeping in a separate user-authored line" $
      -- Only the marked legacy line's values move to the fragment; the user's
      -- own extra-substituters line stays in nix.conf and its values must not
      -- be duplicated into the fragment, even though the marker appears
      -- elsewhere in the same file.
      let nixConf =
            NixConf
              [ Substituters [defaultPublicURI, "https://old.cachix.org"],
                ExtraSubstituters ["https://corp.example"],
                TrustedPublicKeys [defaultSigningKey, "old-key"]
              ]
          result =
            ( NixConf [ExtraSubstituters ["https://corp.example"], cachixInclude],
              NixConf
                [ ExtraSubstituters [defaultPublicURI, "https://old.cachix.org", "https://name.cachix.org"],
                  ExtraTrustedPublicKeys [defaultSigningKey, "old-key", "pub"]
                ]
            )
       in addCache bc nixConf (NixConf []) `shouldBe` result

    it "does not add a second include directive" $
      let nixConf = NixConf [cachixInclude]
       in fst (addCache bc nixConf (NixConf [])) `shouldBe` NixConf [cachixInclude]

    it "removes duplicates" $
      let fragment =
            NixConf
              [ ExtraSubstituters ["bc1", "bc1"],
                ExtraTrustedPublicKeys ["pub1", "pub1"]
              ]
          result =
            ( NixConf [cachixInclude],
              NixConf
                [ ExtraSubstituters ["bc1", "https://name.cachix.org"],
                  ExtraTrustedPublicKeys ["pub1", "pub"]
                ]
            )
       in addCache bc (NixConf [cachixInclude]) fragment `shouldBe` result

  describe "removeCache" $ do
    it "removes a binary cache from the fragment" $
      let nixConf = NixConf [cachixInclude]
          fragment =
            NixConf
              [ ExtraSubstituters ["https://other.example", "https://name.cachix.org"],
                ExtraTrustedPublicKeys ["other.example-1:key", "name.cachix.org-1:key"]
              ]
          result =
            ( ( NixConf [cachixInclude],
                NixConf
                  [ ExtraSubstituters ["https://other.example"],
                    ExtraTrustedPublicKeys ["other.example-1:key"]
                  ]
              ),
              True
            )
       in removeCache "https://cachix.org" "name" nixConf fragment `shouldBe` result

    it "migrates and removes inline settings written by older versions" $
      -- The restated Nix default stays behind in the fragment, preserving
      -- the reachability the old override line guaranteed.
      let nixConf =
            NixConf
              [ Substituters [defaultPublicURI, "https://name.cachix.org"],
                TrustedPublicKeys [defaultSigningKey, "name.cachix.org-1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa="]
              ]
          fragment =
            NixConf
              [ ExtraSubstituters [defaultPublicURI],
                ExtraTrustedPublicKeys [defaultSigningKey]
              ]
       in removeCache "https://cachix.org" "name" nixConf (NixConf [])
            `shouldBe` ((NixConf [cachixInclude], fragment), True)

    it "removes a leftover trusted public key even when the substituter is already gone" $
      let nixConf = NixConf [cachixInclude]
          fragment = NixConf [ExtraTrustedPublicKeys ["name.cachix.org-1:key"]]
       in removeCache "https://cachix.org" "name" nixConf fragment
            `shouldBe` ((NixConf [cachixInclude], NixConf []), True)

    it "does not add an include when the fragment ends up empty" $
      removeCache "https://cachix.org" "name" (NixConf []) (NixConf [ExtraTrustedPublicKeys ["name.cachix.org-1:key"]])
        `shouldBe` ((NixConf [], NixConf []), True)

    it "omits empty extra settings after removal" $
      let nixConf = NixConf [cachixInclude]
          fragment =
            NixConf
              [ ExtraSubstituters ["https://name.cachix.org"],
                ExtraTrustedPublicKeys ["name.cachix.org-1:key"]
              ]
       in removeCache "https://cachix.org" "name" nixConf fragment
            `shouldBe` ((NixConf [cachixInclude], NixConf []), True)

    it "leaves both configs untouched if the binary cache is missing" $
      let nixConf =
            NixConf
              [ Substituters [defaultPublicURI],
                TrustedPublicKeys [defaultSigningKey]
              ]
       in removeCache "https://cachix.org" "name" nixConf (NixConf [])
            `shouldBe` ((nixConf, NixConf []), False)

    it "leaves an unrelated substituters line in nix.conf untouched" $
      let nixConf = NixConf [Substituters ["http"], TrustedPublicKeys ["pub1"]]
       in removeCache "https://cachix.org" "name" nixConf (NixConf [])
            `shouldBe` ((nixConf, NixConf []), False)

  describe "addCacheStandalone" $ do
    it "writes a self-contained nix.conf" $
      addCacheStandalone bc (NixConf [])
        `shouldBe` NixConf
          [ ExtraSubstituters ["https://name.cachix.org"],
            ExtraTrustedPublicKeys ["pub"]
          ]

    it "converts cache lines written by older versions in place" $
      addCacheStandalone
        bc
        ( NixConf
            [ Substituters [defaultPublicURI, "https://other.cachix.org"],
              TrustedPublicKeys [defaultSigningKey, "other-key"]
            ]
        )
        `shouldBe` NixConf
          [ ExtraSubstituters [defaultPublicURI, "https://other.cachix.org", "https://name.cachix.org"],
            ExtraTrustedPublicKeys [defaultSigningKey, "other-key", "pub"]
          ]

  describe "removeCacheStandalone" $ do
    it "removes the cache and its key" $
      removeCacheStandalone
        "https://cachix.org"
        "name"
        ( NixConf
            [ ExtraSubstituters ["https://name.cachix.org", "https://other.example"],
              ExtraTrustedPublicKeys ["name.cachix.org-1:key", "other-1:key"]
            ]
        )
        `shouldBe` ( NixConf
                       [ ExtraSubstituters ["https://other.example"],
                         ExtraTrustedPublicKeys ["other-1:key"]
                       ],
                     True
                   )

    it "reports nothing to remove" $
      removeCacheStandalone "https://cachix.org" "name" (NixConf [ExtraSubstituters ["https://other.example"]])
        `shouldBe` (NixConf [ExtraSubstituters ["https://other.example"]], False)

  describe "parse" $ do
    it "parses substituters" $
      parse "substituters = a\n"
        `shouldBe` Right (NixConf [Substituters ["a"]])

    it "parses extra substituters" $
      parse "extra-substituters = a\n"
        `shouldBe` Right (NixConf [ExtraSubstituters ["a"]])

    it "parses extra trusted public keys" $
      parse "extra-trusted-public-keys = a\n"
        `shouldBe` Right (NixConf [ExtraTrustedPublicKeys ["a"]])

    it "parses long key" $
      parse "binary-caches-parallel-connections = 40\n"
        `shouldBe` Right (NixConf [Other "binary-caches-parallel-connections = 40"])

    it "leaves a line with an inline comment untouched" $
      parse "substituters = a b # mine\n"
        `shouldBe` Right (NixConf [Other "substituters = a b # mine"])

    it "leaves Nix 1.0 alias keys untouched" $
      parse "binary-caches = a\nbinary-cache-public-keys = b\n"
        `shouldBe` Right (NixConf [Other "binary-caches = a", Other "binary-cache-public-keys = b"])

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

  describe "NixConfSource" $ do
    it "write . read" $ do
      withTempDirectory "/tmp" "nixconf" $ \temp -> do
        let confPath = temp </> "nix.conf"
        let subConfPath = temp </> "sub.conf"
        let confContents = "include " <> toS subConfPath <> "\n"
        let parsedConfContents = NixConf [Include (RequiredInclude (toS subConfPath))]
        writeFile confPath confContents
        writeFile subConfPath realExample

        Just conf <- NixConf.read (Custom temp)
        conf `shouldBe` NixConfSource confPath parsedConfContents

        NixConf.write conf
        readFile confPath `shouldReturn` confContents

    -- Test that missing optional includes do not throw errors
    it "resolves required and optional includes" $ do
      withTempDirectory "/tmp" "nixconf" $ \temp -> do
        let confPath = temp </> "nix.conf"
            requiredConfPath = temp </> "required.conf"
            optionalConfPath = temp </> "optional.conf"
            parsedConfContents =
              NixConf
                [ Include (RequiredInclude (toS requiredConfPath)),
                  Include (OptionalInclude (toS optionalConfPath))
                ]
        writeFile confPath $
          unlines
            [ "include " <> toS requiredConfPath <> "\n",
              "!include " <> toS optionalConfPath <> "\n"
            ]
        writeFile requiredConfPath realExample

        Just conf <- NixConf.read (Custom temp)
        NixConf.resolveIncludes conf
          `shouldReturn` [ NixConfSource confPath parsedConfContents,
                           NixConfSource requiredConfPath parsedRealExample
                         ]

    it "detects cycles" $ do
      withTempDirectory "/tmp" "nixconf" $ \temp -> do
        let confPath = temp </> "nix.conf"
            subConfPath = temp </> "sub.conf"
            confContents :: Text = "include " <> toS subConfPath <> "\n"
            subConfContents :: Text = "include " <> toS confPath <> "\n"
        writeFile confPath confContents
        writeFile subConfPath subConfContents

        Just conf <- NixConf.read (Custom temp)

        let isCircularInclude = \case
              CircularInclude _ -> True
              _ -> False
        NixConf.resolveIncludes conf `shouldThrow` isCircularInclude

    it "detects trusted-users through includes" $ do
      withTempDirectory "/tmp" "nixconf" $ \temp -> do
        let confPath = temp </> "nix.conf"
            subConfPath = temp </> "sub.conf"
            confContents :: Text = "include " <> toS subConfPath <> "\n"
            subConfContents :: Text = "trusted-users = @wheel"
        writeFile confPath confContents
        writeFile subConfPath subConfContents

        ncs <- NixConf.resolveIncludes =<< NixConf.readWithDefault (NixConf.Custom temp)
        concatMap (NixConf.readLines NixConf.isTrustedUsers) ncs `shouldBe` ["@wheel"]

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
