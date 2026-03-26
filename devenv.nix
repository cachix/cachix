{
  pkgs,
  lib,
  ghcVersion,
  nixCApiPkgs,
  ...
}:

{
  imports = [ ./git-hooks.nix ];

  packages = [
    pkgs.pkg-config

    # Dependencies
    pkgs.xz
    pkgs.zlib
    pkgs.boost
    pkgs.libsodium

    # Required by nix-util
    pkgs.libblake3
    pkgs.brotli.dev
    pkgs.openssl.dev
    pkgs.curl.dev
    pkgs.sqlite.dev
    pkgs.libgit2
    pkgs.pcre2
  ]
  ++ nixCApiPkgs
  ++ lib.optional pkgs.stdenv.hostPlatform.isx86 pkgs.libcpuid
  ++ [

    # Haskell
    pkgs.cabal-install
    pkgs.stack
    pkgs.haskell.compiler."ghc${ghcVersion}"
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
  ];
}
