# to update: $ nix-prefetch-url --unpack url
let
  fetchTarball = { url, sha256 }@attrs:
    if builtins.lessThan builtins.nixVersion "1.12"
    then builtins.fetchTarball { inherit url; }
    else builtins.fetchTarball attrs;
in import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/d6b74a4145c28f9740c3147c588b122533fcd8f3.tar.gz";
  sha256 = "1qy81fv5drkcdg3830hz17vf4xgkyxc93wm5m2744dyzzkj8fzp7";
}) { config = {}; overlays = []; }
