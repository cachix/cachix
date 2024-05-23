{ pkgs, lib, ghcVersion, getNix, ... }:

{
  imports = [ ./git-hooks.nix ];

  packages = [
    pkgs.pkg-config

    # Dependencies
    pkgs.lzma
    pkgs.zlib
    pkgs.boost
    pkgs.libsodium
    (getNix { inherit pkgs; })

    # Haskell
    pkgs.stack
    pkgs.haskell.compiler."ghc${ghcVersion}"
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
  ]
  ++ lib.optionals pkgs.stdenv.isDarwin [
    pkgs.darwin.apple_sdk.frameworks.Cocoa
    pkgs.darwin.apple_sdk.frameworks.CoreServices
  ];
}
