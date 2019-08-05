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
      cachix = addBuildDepends (generateOptparseApplicativeCompletion "cachix" (justStaticExecutables (filterStack super.cachix))) [ pkgs.boost ];
      cachix-api = filterStack super.cachix-api;
      # TODO: get stack2nix/cabal2nix to fill these in
      inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices;
    };
  };
in hsPkgs.cachix // { 
  hlint = pkgs.hlint; 
  hsPkgs = hsPkgs;
}
