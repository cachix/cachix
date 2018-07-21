# to update: $ nix-prefetch-url --unpack url
let
  fetchTarball = { url, sha256 }@attrs:
    if builtins.lessThan builtins.nixVersion "1.12"
    then builtins.fetchTarball { inherit url; }
    else builtins.fetchTarball attrs;
in import (fetchTarball {
  url = "https://github.com/domenkozar/nixpkgs/archive/ed4f1af5efc2d78977fe90a7a8ecf84ffd7d1217.tar.gz";
  sha256 = "0jsc3mqwvy2vlbc3zkycv4c5w8z122fzf3ia31mpbq75nknaa7nd";
}) { config = {}; overlays = []; }
