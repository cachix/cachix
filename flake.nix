{
  description = "CLI for Hosted Nix binary caches";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.flake-compat.follows = "flake-compat";
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

  outputs = { self, nixpkgs, hnix-store-core, ... }@inputs: let
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

  in
    {
      packages = forAllSystems (system: 
        let
          pkgs = import nixpkgs { inherit system; };
          inherit (customHaskellPackages { inherit pkgs; inherit (pkgs) haskellPackages; })
            cachix;
        in
        {
          cachix = pkgs.haskell.lib.justStaticExecutables cachix;
          default = pkgs.haskell.lib.justStaticExecutables cachix;
          ci = self.devShell.${system}.ci;
        });

      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in inputs.devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [{
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

            pre-commit.hooks = {
              cabal-fmt.enable = true;
              hlint.enable = true;
              ormolu.enable = true;
              ormolu.excludes = [ "Cachix/Client/Servant\.hs$" "Cachix/Client/Config/Orphans\.hs$" "Cachix/Types/SwaggerOrphans\.hs$" ];
              shellcheck.enable = true;
            };
        }];
        }
      );

      lib = {
        # Let downstream haskell packages such as hercules-ci-agent use the
        # overrides we declare in their CI.
        inherit customHaskellPackages;
      };
    };
}
