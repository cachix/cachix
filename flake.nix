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
    secretspec = {
      url = "github:cachix/secretspec/v0.16.0";
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

      secretspecVersion =
        (builtins.fromTOML (builtins.readFile (inputs.secretspec + "/Cargo.toml")))
        .workspace.package.version;

      # The native resolver behind the secretspec Haskell SDK. Only the cdylib
      # is installed: the GHC RTS linker aborts loading the Rust staticlib
      # during Template Haskell on aarch64 (CHECK(bssBegin <= bssEnd) in
      # rts/linker/Elf.c), while the shared library is loaded by the system
      # dynamic linker and ends up in the closure like any other C library.
      getSecretspecFfi =
        pkgs:
        pkgs.rustPlatform.buildRustPackage {
          pname = "secretspec-ffi";
          version = secretspecVersion;
          src = inputs.secretspec;
          cargoLock.lockFile = inputs.secretspec + "/Cargo.lock";
          buildAndTestSubdir = "secretspec-ffi";
          # Embed libdbus so the library has no runtime dbus dependency.
          buildFeatures = nixpkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ "vendored-dbus" ];
          doCheck = false;
          postInstall = ''
            rm -f $out/lib/libsecretspec_ffi.a
          '';
        };

      customHaskellPackages =
        {
          pkgs,
          haskellPackages ? pkgs.haskellPackages,
        }@args:
        let
          hlib = pkgs.haskell.lib;
          cachix-api = haskellPackages.callCabal2nix "cachix-api" ./cachix-api { };

          # Resolve CACHIX_AUTH_TOKEN via secretspec (https://secretspec.dev).
          # Linux-only for now: on darwin the Rust archive needs Security
          # framework link flags that are not wired up yet.
          withSecretspec = pkgs.stdenv.hostPlatform.isLinux;

          secretspec = hlib.dontCheck (
            haskellPackages.callCabal2nix "secretspec" (inputs.secretspec + "/secretspec-hs") {
              secretspec_ffi = getSecretspecFfi pkgs;
            }
          );

          cachix =
            hlib.overrideCabal
              (haskellPackages.callCabal2nixWithOptions "cachix" ./cachix
                (nixpkgs.lib.optionalString withSecretspec "--flag=secretspec")
                (
                  {
                    inherit cachix-api;
                    hnix-store-core = haskellPackages.hnix-store-core_0_8_0_0 or haskellPackages.hnix-store-core;
                    nix = getNix args;
                  }
                  // nixpkgs.lib.optionalAttrs withSecretspec { inherit secretspec; }
                )
              )
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
              ({
                _module.args = {
                  inherit ghcVersion getNix;
                  secretspecFfi = getSecretspecFfi pkgs;
                };
              })
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
