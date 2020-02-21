{
  cachix = import ./default.nix;
  pre-commit-check = (import ./nix {}).pre-commit-check;
}
