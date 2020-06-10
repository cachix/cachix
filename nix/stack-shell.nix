{ ghc }:
let 
  sources = (import ./sources.nix);
  pkgs = import sources.nixpkgs {} ;
in pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "cachix-stack-shell";
  buildInputs = [
    pkgs.lzma
    pkgs.zlib
    pkgs.nix
    pkgs.boost
    pkgs.libsodium
  ];
}
