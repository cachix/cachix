let
  sources = (import ./nix/sources.nix);
  pin = import sources.nixpkgs {} ;
  haskell = import (sources."haskell.nix") { pkgs = pin; };
  gitignoreSource = (import sources.gitignore { inherit (pin) lib; }).gitignoreSource;

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import (haskell.callStackToNix {
      src = gitignoreSource ./.;
    });
    pkg-def-extras = [];
    modules = [
     { packages.Cabal.patches = [./nix/cabal.patch]; }
     { packages.happy.package.setup-depends = [pkgSet.config.hsPkgs.Cabal]; }
     { packages.pretty-show.package.setup-depends = [pkgSet.config.hsPkgs.Cabal]; }
    ];
  };
  packages = pkgSet.config.hsPkgs;
in packages.cachix.components.exes.cachix
