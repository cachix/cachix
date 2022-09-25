{ system ? builtins.currentSystem
, pkgs ? import ./nix { inherit system; }
}:
let
  # TODO: Choose between stack and cabal. Don't use both!
  # GHC version should match nixpkgs to test potential release build
  haskellPackages = pkgs.haskellPackages;

  cachix-api = haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};

  cachix = haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) {
    inherit cachix-api;
    # Make sure that the nix version here is within the range supported by hercules-ci-cnix-store
    nix = pkgs.nixVersions.nix_2_9;
  };
in
pkgs.haskell.lib.justStaticExecutables cachix
