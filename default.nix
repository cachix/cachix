{
  system ? builtins.currentSystem,
  pkgs ? import ./nix { inherit system; },
}:
let

  cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
  cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api hercules-ci-cnix-store; nix = pkgs.nix_2_3 or pkgs.nix;};

  withNixUnstable =
    import ./default.nix { pkgs = pkgs.extend (self: super: { nix = self.nixUnstable; }); };

  addNixVersionFlag = pkg:
    pkgs.haskell.lib.overrideCabal pkg (o: {
      preConfigure = (o.preConfigure or "") + ''
        if pkg-config --atleast-version 2.4pre nix-store; then
          configureFlags="$configureFlags --flag nix-2_4"
        fi
      '';
    });
  hercules-ci-cnix-store = pkgs.haskell.lib.addBuildDepends (
    addNixVersionFlag pkgs.haskellPackages.hercules-ci-cnix-store
  ) [ pkgs.nlohmann_json ];

in pkgs.haskell.lib.justStaticExecutables cachix // { inherit withNixUnstable; }
