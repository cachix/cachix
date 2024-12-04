{ pkgs, ghcVersion, getNix, ... }:

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

    # Haskell
    pkgs.stack
    pkgs.haskell.compiler."ghc${ghcVersion}"
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
  ];
}
