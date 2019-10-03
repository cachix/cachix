let
  pkgs = import ./nix {};
  src = pkgs.gitignoreSource ./.;
  pkgSet = pkgs.haskellnix.mkStackPkgSet {
    stack-pkgs = import (pkgs.haskellnix.callStackToNix {
      inherit src;
    });
    pkg-def-extras = [];
    modules = [
     { packages.Cabal.patches = [./nix/cabal.patch]; }
     { packages.happy.package.setup-depends = [pkgSet.config.hsPkgs.Cabal]; }
     { packages.pretty-show.package.setup-depends = [pkgSet.config.hsPkgs.Cabal]; }
     { packages.cachix.components.library.build-tools = [ pkgs.boost ]; }
    ];
  };
  packages = pkgSet.config.hsPkgs;
in packages.cachix.components.all  // {
  inherit (pkgs) pre-commit-check;
}
