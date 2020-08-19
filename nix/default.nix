{ sources ? import ./sources.nix, system ? builtins.currentSystem }:
let
  overlay = self: pkgs: {
    inherit (import sources.niv {}) niv;
    pre-commit-hooks-nix = import sources."pre-commit-hooks.nix";
    inherit (import sources.gitignore { inherit (pkgs) lib; })
      gitignoreSource;
    nix-store = pkgs.nix;
    nix-main = pkgs.nix;
    boost_context = pkgs.boost;
    pre-commit-check = self.pre-commit-hooks-nix.run {
      hooks.cabal-fmt.enable = true;
      hooks.hlint.enable = true;
      hooks.ormolu.enable = true;
      hooks.ormolu.excludes = ["Cachix/Client/Servant\.hs$" "Cachix/Client/Config/Orphans\.hs$" "Cachix/Types/SwaggerOrphans\.hs$" ];
      hooks.shellcheck.enable = true;
      src = self.gitignoreSource ../.;
    };
  };
in import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; inherit system; }
