{
  pkgs,
  ghcVersion,
  getNix,
  inputs,
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
    (getNix { inherit pkgs; })

    # Required by nix-util
    pkgs.libblake3
    pkgs.brotli.dev
    pkgs.openssl.dev
    pkgs.curl.dev
    pkgs.sqlite.dev
    pkgs.libgit2
    pkgs.pcre2

    # Haskell
    pkgs.stack
    # Works
    inputs.ghc.legacyPackages.${pkgs.stdenv.system}.haskell.compiler.ghc9101
    # Broken
    # inputs.ghc.legacyPackages.${pkgs.stdenv.system}.haskell.compiler.ghc9102
    #
    # pkgs.haskell.compiler."ghc${ghcVersion}"
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
  ];
}
