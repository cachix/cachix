{ pkgs ? import ./nixpkgs.nix }:
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    # hie can't find hspec-discover via stack
    pkgs.haskellPackages.hspec-discover
    ];
  NIX_PATH = "nixpkgs=${pkgs.path}";
}
