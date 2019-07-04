{ ghc }:
let pkgs = import ../nixpkgs.nix;
in pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "cachix-stack-shell";
  buildInputs = [
    pkgs.lzma
    pkgs.zlib
  ];
}
