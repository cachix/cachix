{
  description = "CLI for Hosted Nix binary caches";

  nixConfig = {
    extra-substituters = "https://cachix.cachix.org";
    extra-trusted-public-keys = "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-compat.follows = "flake-compat";
    };
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-compat.follows = "flake-compat";
      inputs.git-hooks.follows = "git-hooks";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, git-hooks, ... }@inputs: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs systems;

    # Keep the stable version in sync with:
    #   - the stackage release in nixpkgs
    #   - the GHC version in nixpkgs-<stable>
    stableGhcVersion = "98";
    # Test builds against the latest GHC version in nixpkgs
    latestGhcVersion = "HEAD";

    # Try to use the same Nix version as cnix-store, if available.
    getNix = { pkgs, haskellPackages ? pkgs.haskellPackages }:
      haskellPackages.hercules-ci-cnix-store.nixPackage or pkgs.nix;

    # Build our packages with overrides applied.
    # Provide `haskellPackages` to override the GHC version.
    customHaskellPackages = { pkgs, haskellPackages }: rec {
      cachix-api = haskellPackages.callCabal2nix "cachix-api" ./cachix-api {};

      cachix = haskellPackages.callCabal2nix "cachix" ./cachix {
        inherit cachix-api;
        hnix-store-core = haskellPackages.hnix-store-core_0_8_0_0 or haskellPackages.hnix-store-core;
        nix = getNix { inherit pkgs haskellPackages; };
      };
    };

    mkPackages = { system, ghcVersion, suffix ? "" }:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;
        hlib = pkgs.haskell.lib;
        # Use haskell packages for the specified GHC version
        haskellPackages = pkgs.haskell.packages."ghc${ghcVersion}";
        inherit (customHaskellPackages { inherit pkgs; inherit haskellPackages; }) cachix cachix-api;
        cachixPackages = {
          inherit cachix-api;
          cachix = hlib.justStaticExecutables cachix;
        };
        releasePackages = {
          cachix = hlib.sdistTarball cachix;
          cachix-api = hlib.sdistTarball cachix-api;
        };
        mapAttrNames = f: xs: lib.mapAttrs' (name: value: lib.nameValuePair (f name) value) xs;
      in mapAttrNames (name: name + suffix) {
        inherit (cachixPackages) cachix cachix-api;
        release = pkgs.symlinkJoin {
          name = "cachix-release" + suffix;
          paths = lib.attrValues releasePackages;
        };
        all-packages = pkgs.symlinkJoin {
          name = "cachix-packages" + suffix;
          paths = lib.attrValues cachixPackages;
        };
      };
  in
    {
      packages = forAllSystems (system:
        let
          stablePackages = mkPackages { inherit system; ghcVersion = stableGhcVersion; };
          latestPackages = mkPackages { inherit system; ghcVersion = latestGhcVersion; suffix = "-ghc-latest"; };
        in
          stablePackages // latestPackages // {
            ci = self.devShells.${system}.default.ci;
            default = stablePackages.cachix;
          }
      );

      checks = forAllSystems (system: {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          inherit ((import ./git-hooks.nix).pre-commit) hooks;
        };
      });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = inputs.devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ _module.args = { inherit getNix; ghcVersion = stableGhcVersion; }; })
              ./devenv.nix
            ];
          };
        }
      );

      lib = {
        # Let downstream haskell packages such as hercules-ci-agent use the
        # overrides we declare in their CI.
        inherit customHaskellPackages;
      };
    };
}
