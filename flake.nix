{
  description = "CLI for Hosted Nix binary caches";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd8bbdd4fdb15f7758fd3c8df531e9d42bee239d";
    flake-utils.url = "github:numtide/flake-utils/c0e246b9b83f637f4681389ecabcb2681b4f3af0";
    # https://discourse.nixos.org/t/recommendations-for-use-of-flakes-input-follows/17413
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    hls = {
      url = "github:haskell/haskell-language-server/255498147abb1b18a9cae3c61c5558d7fc84ab31";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    niv = {
      url = "github:nmattia/niv/82e5cd1ad3c387863f0545d7591512e76ab0fc41";
      flake = false;
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix/8cb8ea5f1c7bc2984f460587fddd5f2e558f6eb8";
      inputs.flake-utils.follows = "flake-utils";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat/b4a34015c698c7793d592d66adbab377907a2be8";
      flake = false;
    };
    my-codium = {
      url = "github:br4ch1st0chr0n3/flakes?dir=codium&rev=5317c7a3882a96bc8ee92bb4979edaac6047a183";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , hls
    , niv
    , pre-commit-hooks-nix
    , gitignore
    , flake-compat
    , my-codium
    }:
      with flake-utils;
      lib.eachDefaultSystem (system:
      let
        # https://discourse.nixos.org/t/using-nixpkgs-legacypackages-system-vs-import/17462
        pkgs = nixpkgs.legacyPackages.${system};

        ### Haskell

        # need to match Stackage LTS version
        # from stack.yaml resolver
        ghcVersion = "924";
        hPkgs = pkgs.haskell.packages."ghc${ghcVersion}";
        tools =
          let
            inherit (my-codium.packages.${system})
              writeSettingsJson
              settingsNix
              toList
              shellTools
              mkCodium
              vscodeExtensions
              ;
          in
          pkgs.lib.lists.flatten
            [
              (toList {
                inherit (shellTools) nix;
                haskell = builtins.removeAttrs shellTools.haskell [ "haskell-language-server" ];
              })
              (writeSettingsJson (settingsNix // {
                window."window.zoomLevel" = 0.3;
              }))
              (mkCodium (vscodeExtensions // {
                nix = { inherit (vscodeExtensions.nix) direnv nix-ide; };
              }))
              (pkgs.bashInteractive)
              hls.packages.${system}."haskell-language-server-${ghcVersion}"
            ];

        inherit (gitignore.lib) gitignoreSource;
        pre-commit-check = pre-commit-hooks-nix.lib.${system}.run {
          hooks.cabal-fmt.enable = true;
          hooks.hlint.enable = true;
          hooks.ormolu.enable = true;
          hooks.ormolu.excludes = [ "Cachix/Client/Servant\.hs$" "Cachix/Client/Config/Orphans\.hs$" "Cachix/Types/SwaggerOrphans\.hs$" ];
          hooks.shellcheck.enable = true;
          src = gitignoreSource ./.;
        };

        ### Cachix

        cachix =
          let
            inherit (hPkgs) callCabal2nix;
            inherit (pkgs.haskell.lib) justStaticExecutables;

            cachix-api = callCabal2nix "cachix-api" (gitignoreSource ./cachix-api) { };
            cachix = callCabal2nix "cachix" (gitignoreSource ./cachix) {
              inherit cachix-api;
              # note: this has to be in sync with what hercules-ci-cnix-store was compiled with
              nix = pkgs.nixVersions.nix_2_9;
            };
          in
          justStaticExecutables cachix;
      in
      {
        packages = {
          default = cachix;
        };

        inherit pre-commit-check;

        devShells = {
          default = pkgs.mkShell {
            buildInputs = tools;

            # https://stackoverflow.com/a/63751678
            shellHook = ''
              export LANG="C.UTF-8"
              write-settings
              ${pre-commit-check.shellHook}
            '';

            # Make external Nix c libraries like zlib known to GHC, like
            # pkgs.haskell.lib.buildStackProject does
            # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath tools;
            NIX_PATH = "nixpkgs=${pkgs.path}";
          };
        };

        stack-shell = { ghcVersion }:

          pkgs.haskell.lib.buildStackProject {
            name = "cachix-stack-shell";

            ghc = pkgs.haskell.compiler.${ghcVersion};

            buildInputs = [
              pkgs.lzma
              pkgs.zlib
              pkgs.nixVersions.nix_2_10
              pkgs.boost
            ];
          };

      });

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
    ];
  };
}
