{ pkgs ? import ./nixpkgs.nix
}:

pkgs.haskell.lib.justStaticExecutables (import ./stack2nix.nix { inherit pkgs; }).cachix
