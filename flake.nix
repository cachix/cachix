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
      url = "github:cachix/devenv";
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
      hnix-store-core =
        let
          src = pkgs.fetchFromGitHub {
            owner = "sandydoo";
            repo = "hnix-store";
            rev = "d705ab961d23cbe3ebb8572e6d49ada43e86cbb2";
            hash = "sha256-Vc7Jah2jlgLawS4m1WA5FpxW2vODbeMJ5RCgS5lvQoA=";
          };

          hlib = pkgs.haskell.lib;
        in
        pkgs.lib.pipe
          haskellPackages.hnix-store-core_0_6_1_0
          [
            (hlib.compose.overrideSrc { src = "${src}/hnix-store-core"; })
            (hlib.compose.addBuildDepend haskellPackages.case-insensitive)
          ];
      cachix-api = haskellPackages.callCabal2nix "cachix-api" ./cachix-api {};
      cachix = haskellPackages.callCabal2nix "cachix" ./cachix {
        inherit cachix-api hnix-store-core;
        fsnotify = haskellPackages.fsnotify_0_4_1_0 or haskellPackages.fsnotify;
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
          inherit (customHaskellPackages { inherit pkgs; inherit (pkgs) haskellPackages; })
            cachix;
        in
        {
          cachix = pkgs.haskell.lib.justStaticExecutables cachix;
          default = pkgs.haskell.lib.justStaticExecutables cachix;
          ci = self.devShells.${system}.default.ci;
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
            pkgs.haskell.compiler.ghc946
            (pkgs.haskell-language-server.override { supportedGhcVersions = [ "946" ]; })
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.CoreServices
          ];
        in
        rec {
          default = devenv;

          # Temporary nixpkgs.mkShell until the LD_LIBRARY issues in devenv are resolved
          nixDevShell = pkgs.mkShell {
            inherit packages;
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };

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
