{
  git-hooks.hooks = {
    cabal-fmt.enable = true;
    cabal2nix.enable = true;
    hlint.enable = true;
    nixfmt = {
      enable = true;
      # Keep generated expressions in cabal2nix's format.
      excludes = [ "^cachix(-api)?/default\\.nix$" ];
    };
    ormolu.enable = true;
    shellcheck.enable = true;
  };
}
