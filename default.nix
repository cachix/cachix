let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { overlays = [(self: super: { nix-store = self.nix; nix-main = self.nix; })];} ;
  haskell = import sources."haskell.nix" { inherit pkgs; };
  inherit (import sources.gitignore { inherit (pkgs) lib; })
    gitignoreSource;

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import (haskell.callStackToNix {
      src = gitignoreSource ./.;
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
in packages.cachix.components.exes.cachix 
