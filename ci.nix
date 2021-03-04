{ src ? null
, multiSystemBuild ? src != null # true for Hercules CI
, system ? builtins.currentSystem
}:
let
  ci = system: _: {
    cachix = import ./default.nix { inherit system; };
    pre-commit-check = (import ./nix { inherit system; }).pre-commit-check;
    recurseForDerivations = true;
  };

  ciSystems = {
    "x86_64-linux" = {};
    "aarch64-linux" = {};
    "x86_64-darwin" = {};
  };
in
  if multiSystemBuild
  then builtins.mapAttrs ci ciSystems
  else ci system {}
