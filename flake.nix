{
  description = "CLI for Hosted Nix binary caches";

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
    hnix-store-core = {
      url = "github:haskell-nix/hnix-store/core-0.6.1.0";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, hnix-store-core, ... }@inputs: let
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
        fsnotify = haskellPackages.fsnotify_0_4_1_0;
        hnix-store-core = haskellPackages.callCabal2nix "hnix-store-core" "${hnix-store-core}/hnix-store-core" {};
        nix = getNix { inherit pkgs haskellPackages; };
      };
    };

    preCommitHooks = {
      cabal-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
      ormolu.excludes = [ "Cachix/Client/Servant\.hs$" "Cachix/Client/Config/Orphans\.hs$" "Cachix/Types/SwaggerOrphans\.hs$" ];
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
            pkgs.haskell.compiler.ghc927
            (pkgs.haskell-language-server.override { supportedGhcVersions = [ "927" ]; })
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
