{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix { inherit system; };
  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api; };
in pkgs.haskell.lib.disableLibraryProfiling cachix
