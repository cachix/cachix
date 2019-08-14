let
  sources = (import ./nix/sources.nix);
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    # hie can't find hspec-discover via stack
    pkgs.haskellPackages.hspec-discover
    ];
  NIX_PATH = "nixpkgs=${pkgs.path}";
}
