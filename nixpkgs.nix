# to update: $ nix-prefetch-url --unpack url
import (builtins.fetchTarball {
  url = "https://github.com/domenkozar/nixpkgs/archive/ed4f1af5efc2d78977fe90a7a8ecf84ffd7d1217.tar.gz";
  sha256 = "0jsc3mqwvy2vlbc3zkycv4c5w8z122fzf3ia31mpbq75nknaa7nd";
}) { config = {}; overlays = []; }
