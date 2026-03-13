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

  outputs =
    {
      self,
      nixpkgs,
      git-hooks,
      ...
    }@inputs:
    let
      systems = [
        "x86_64-linux"
        "i686-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # Keep in sync with stack.yaml
      ghcVersion = "910";

      # Try to use the same Nix version as cnix-store, if available.
      getNix =
        {
          pkgs,
          haskellPackages ? pkgs.haskellPackages,
        }:
        haskellPackages.hercules-ci-cnix-store.nixPackage or pkgs.nix;

      customHaskellPackages =
        {
          pkgs,
          haskellPackages ? pkgs.haskellPackages,
        }@args:
        let
          hlib = pkgs.haskell.lib;
          # CA derivation FFI bindings (hercules-ci/hercules-ci-agent#670)
          hercules-ci-cnix-store-src =
            let
              src =
                builtins.fetchGit {
                  url = "https://github.com/hercules-ci/hercules-ci-agent";
                  rev = "a8a8acd6c77179963da9814f82ca10e61425a0a0";
                }
                + "/hercules-ci-cnix-store";
            in
            pkgs.runCommand "hercules-ci-cnix-store-src" { } ''
              cp -r ${src} $out
              chmod -R u+w $out
              cat > $out/include/hercules-ci-cnix/store.hxx << 'HEADER'
              #pragma once

              #include <nix/store/path-info.hh>
              #include <nix/store/derived-path-map.hh>

              typedef nix::ref<nix::Store> refStore;

              typedef nix::Strings::iterator StringsIterator;
              typedef nix::DerivationOutputs::iterator DerivationOutputsIterator;
              typedef nix::StringPairs::iterator StringPairsIterator;
              typedef nix::PathSet::iterator PathSetIterator;
              typedef nix::ref<const nix::ValidPathInfo> refValidPathInfo;

              typedef nix::DerivedPathMap<std::set<nix::OutputName, std::less<>>>::Map::iterator DerivationInputsIterator;
              HEADER
            '';
          hercules-ci-cnix-store =
            haskellPackages.callCabal2nix "hercules-ci-cnix-store" hercules-ci-cnix-store-src
              { nix = pkgs.nix; };
          cachix-api = haskellPackages.callCabal2nix "cachix-api" ./cachix-api { };
          cachix =
            hlib.overrideCabal
              (haskellPackages.callCabal2nix "cachix" ./cachix {
                inherit cachix-api hercules-ci-cnix-store;
                hnix-store-core = haskellPackages.hnix-store-core_0_8_0_0 or haskellPackages.hnix-store-core;
                nix = pkgs.nix;
              })
              # Apply a fix for a bug in GHC 9.10.3 that fails to load libraries using weak references on macOS 26.
              # https://github.com/NixOS/nixpkgs/pull/469906
              (
                old: {
                  preBuild = ''
                    DYLD_INSERT_LIBRARIES="''${DYLD_INSERT_LIBRARIES:+$DYLD_INSERT_LIBRARIES:}$(pkg-config --variable=libdir nix-store)/libnixstore.dylib:$(pkg-config --variable=libdir nix-util)/libnixutil.dylib"
                    export DYLD_INSERT_LIBRARIES
                    echo "DYLD_INSERT_LIBRARIES=$DYLD_INSERT_LIBRARIES"
                  ''
                  + (old.preBuild or "");
                }
              );
        in
        {
          inherit cachix cachix-api;
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hlib = pkgs.haskell.lib;
          inherit (customHaskellPackages { inherit pkgs; })
            cachix
            cachix-api
            ;
          release = {
            cachix = hlib.sdistTarball cachix;
            cachix-api = hlib.sdistTarball cachix-api;
          };
        in
        {
          cachix = hlib.justStaticExecutables cachix;
          release = pkgs.symlinkJoin {
            name = "release";
            paths = builtins.attrValues release;
          };
        }
        // {
          ci = self.devShells.${system}.default.ci;
          default = self.packages.${system}.cachix;
        }
      );

      checks = forAllSystems (system: {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          inherit ((import ./git-hooks.nix).pre-commit) hooks;
        };
      });

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = inputs.devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ _module.args = { inherit ghcVersion getNix; }; })
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
