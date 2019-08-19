let
  pkgs = import ./nix {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.niv
    # hie can't find hspec-discover via stack
    pkgs.haskellPackages.hspec-discover
    ];
  NIX_PATH = "";
  inherit (pkgs.pre-commit-check) shellHook;
}
