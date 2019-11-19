{ sources ? import ./sources.nix }:
let
  overlay = self: pkgs: {
    inherit (import sources.niv {}) niv;
    nix-pre-commit-hooks = import sources.nix-pre-commit-hooks;
    haskellnix = import sources."haskell.nix" { pkgs = self; };
    inherit (import sources.gitignore { inherit (pkgs) lib; })
      gitignoreSource;
    nix-store = pkgs.nix;
    nix-main = pkgs.nix;
    boost_context = pkgs.boost;
    pre-commit-check = self.nix-pre-commit-hooks.run {
      src = self.gitignoreSource ../.;
      hooks.ormolu.enable = true;
      hooks.ormolu.excludes = ["Cachix/Client/Servant\.hs$" "Cachix/Types/SwaggerOrphans\.hs$" ];
      hooks.shellcheck.enable = true;
      hooks.hlint.enable = true;
      hooks.cabal-fmt.enable = true;
    };
  };
in import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
