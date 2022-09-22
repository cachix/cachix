{ ghcVersion }:

let
  sources = (import ./sources.nix);
  pkgs = import sources.nixpkgs {};
in
pkgs.haskell.lib.buildStackProject {
  name = "cachix-stack-shell";

  ghc = pkgs.haskell.compiler."${ghcVersion}";

  NIX_PATH = "nixpkgs=${pkgs.path}";

  buildInputs = [
    pkgs.lzma
    pkgs.zlib
    # TODO: match this to version in default.nix
    pkgs.nixVersions.nix_2_9
    pkgs.boost
  ];
}
