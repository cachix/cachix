let
  pkgs = import ./nix {};
  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api; };
in pkgs.haskell.lib.disableLibraryProfiling cachix
