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
      # TODO: get stack2nix/cabal2nix to fill these in
      inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices;
    };
  };
in if builtins.compareVersions "2.0" builtins.nixVersion == 1
   then abort ''
      Cachix requires Nix >= 2.0, please upgrade:
      - If you are running NixOS, use `nixos-rebuild' to upgrade your system.
      - If you installed Nix using the install script (https://nixos.org/nix/install),
        it is safe to upgrade by running it again:
            curl https://nixos.org/nix/install | sh
   ''
  else hsPkgs.cachix
