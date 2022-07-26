let
  pkgs = import ./nix {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.niv
    # hie can't find hspec-discover via stack
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.releaser
    pkgs.haskellPackages.hkgr
  ];
  NIX_PATH = "nixpkgs=${pkgs.path}";
  inherit (pkgs.pre-commit-check) shellHook;
}
