{
  description = "CLI for Hosted Nix binary caches";

  nixConfig = {
    extra-substituters = "https://cachix.cachix.org";
    extra-trusted-public-keys = "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv = {
      url = "github:cachix/devenv/python-rewrite";
      inputs.flake-compat.follows = "flake-compat";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, ... }@inputs: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);

    # Try to use the same Nix version as cnix-store, if available.
    getNix = { pkgs, haskellPackages ? pkgs.haskellPackages }:
      haskellPackages.hercules-ci-cnix-store.nixPackage
        or pkgs.nixVersions.nix_2_9;

    customHaskellPackages = { pkgs, haskellPackages }: rec {
      cachix-api = haskellPackages.callCabal2nix "cachix-api" ./cachix-api {};
      cachix = haskellPackages.callCabal2nix "cachix" ./cachix {
        inherit cachix-api;
        fsnotify = haskellPackages.fsnotify_0_4_1_0 or haskellPackages.fsnotify;
        hnix-store-core =
          haskellPackages.hnix-store-core_0_7_0_0
          or haskellPackages.hnix-store-core_0_6_1_0
          or haskellPackages.hnix-store-core;
        nix = getNix { inherit pkgs haskellPackages; };
      };
    };

    preCommitHooks = {
      cabal-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
      shellcheck.enable = true;
    };
  in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hlib = pkgs.haskell.lib;
          inherit (customHaskellPackages { inherit pkgs; inherit (pkgs) haskellPackages; })
            cachix cachix-api;
          release = {
            cachix = hlib.sdistTarball cachix;
            cachix-api = hlib.sdistTarball cachix-api;
          };
        in
        {
          cachix = pkgs.haskell.lib.justStaticExecutables cachix;
          default = pkgs.haskell.lib.justStaticExecutables cachix;
          ci = self.devShells.${system}.default.ci;
          release = pkgs.symlinkJoin { name = "release"; paths = builtins.attrValues release; };
        });

      checks = forAllSystems (system: {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = preCommitHooks;
        };
      });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          packages = [
            pkgs.lzma
            pkgs.zlib
            pkgs.boost
            pkgs.stack
            pkgs.pkg-config
            pkgs.libsodium
            (getNix { inherit pkgs; })
            # sync with stack.yaml LTS
            pkgs.haskell.compiler.ghc948
            (pkgs.haskell-language-server.override { supportedGhcVersions = [ "948" ]; })
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.CoreServices
          ];
        in
        rec {
          default = devenv;

          devenv = inputs.devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [{
              inherit packages;
              pre-commit.hooks = preCommitHooks;
            }];
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
