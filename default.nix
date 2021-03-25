{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix { inherit system; };
  hercules-ci-cnix-store = pkgs.haskellPackages.callCabal2nix "hercules-ci-cnix-store" (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "hercules-ci-agent";
    rev = "f44727ea5b9289d98e4976c634486cb15bcc6b52";
    sha256 = "1pgm8mqdxj6sjc318vpifqslpfadwqj252xysf65w2iff2d9qbkz";
  } + "/hercules-ci-cnix-store") {};
  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api hercules-ci-cnix-store; };
in cachix
