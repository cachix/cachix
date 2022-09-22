{ system ? builtins.currentSystem
, pkgs ? import ./nix { inherit system; }
}:
let
  # Match the GHC version to the one chosen by Stack.
  # TODO: Choose between stack and cabal. Don't use both!
  haskellPackages = pkgs.haskell.packages.ghc924;

  cachix-api = haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};

  cachix = haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) {
    inherit cachix-api;
    # Make sure that the nix version here is within the range supported by hercules-ci-cnix-store
    nix = pkgs.nixVersions.nix_2_9;
  };
in
pkgs.haskell.lib.justStaticExecutables cachix
