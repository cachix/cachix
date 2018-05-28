{ pkgs ? import ./nixpkgs.nix
}:

with pkgs.haskell.lib;

let
  filterStack = drv: overrideCabal drv (super: {
     src = builtins.filterSource
      (path: type: baseNameOf path != ".stack-work")
      super.src;
    }
  );
  hsPkgs = (import ./stack2nix.nix { inherit pkgs; }).override {
    overrides = self: super: {
      cachix = justStaticExecutables (filterStack super.cachix);
      cachix-api = filterStack super.cachix-api;
    };
  };
in hsPkgs.cachix
