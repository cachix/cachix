{
  description = "CLI for Hosted Nix binary caches";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.flake-compat.follows = "flake-compat";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);

    customHaskellPackages = { pkgs, haskellPackages }: rec {
      cachix-api = haskellPackages.callCabal2nix "cachix-api" ./cachix-api {};
      cachix = haskellPackages.callCabal2nix "cachix" ./cachix {
        inherit cachix-api;
        fsnotify = haskellPackages.fsnotify_0_4_1_0;
        hnix-store-core = haskellPackages.hnix-store-core_0_6_1_0;
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
              # sync with stack.yaml LTS
              pkgs.haskell.compiler.ghc927
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
