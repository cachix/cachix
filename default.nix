let
  pkgs = import ./nix {};
  src = pkgs.gitignoreSource ./.;
  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api; };
in cachix // {
  inherit (pkgs) pre-commit-check;
}
