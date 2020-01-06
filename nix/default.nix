{ sources ? import ./sources.nix }:
let
  overlay = self: pkgs: {
    inherit (import sources.niv {}) niv;
    pre-commit-hooks-nix = import sources."pre-commit-hooks.nix";
    haskellnix = import sources."haskell.nix" { pkgs = self; };
    inherit (import sources.gitignore { inherit (pkgs) lib; })
      gitignoreSource;
    nix-store = pkgs.nix;
    nix-main = pkgs.nix;
    boost_context = pkgs.boost;
    pre-commit-check = self.pre-commit-hooks-nix.run {
      hooks.cabal-fmt.enable = true;
      hooks.hlint.enable = true;
      hooks.ormolu.enable = true;
      hooks.ormolu.excludes = ["Cachix/Client/Servant\.hs$" "Cachix/Types/SwaggerOrphans\.hs$" ];
      hooks.shellcheck.enable = true;
      src = self.gitignoreSource ../.;
    };
  };
in import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
