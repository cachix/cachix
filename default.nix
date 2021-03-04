{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix { inherit system; };
  hercules-ci-cnix-store = pkgs.haskellPackages.callCabal2nix "hercules-ci-cnix-store" (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "hercules-ci-agent";
    rev = "1af198dd5e9f0f895e52df03a4de46b1e9a98900";
    sha256 = "0wbhycmh5pvk3c56x4gmdqshcww443mglb6hhy35xbqzpqvmnflm";
  } + "/hercules-ci-cnix-store") {};
  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api hercules-ci-cnix-store; };
in cachix
