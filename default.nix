{
  system ? builtins.currentSystem,
  pkgs ? import ./nix { inherit system; },
}:
let

  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api; };

in pkgs.haskell.lib.justStaticExecutables cachix
