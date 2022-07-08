{ system ? builtins.currentSystem
, pkgs ? import ./nix { inherit system; }
,
}:
let

  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) {
    inherit cachix-api;
    # note: this has to be in sync with what hercules-ci-cnix-store was compiled with
    nix = pkgs.nixVersions.nix_2_7;
  };

in
pkgs.haskell.lib.justStaticExecutables cachix
