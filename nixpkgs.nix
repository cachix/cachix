# to update: $ nix-prefetch-url --unpack url
let
  fetchTarball = { url, sha256 }@attrs:
    if builtins.lessThan builtins.nixVersion "1.12"
    then builtins.fetchTarball { inherit url; }
    else builtins.fetchTarball attrs;
in import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/981fdb42074fe961b30624b3b738e3c77a2d0a96.tar.gz";
  sha256 = "1fnqscn89w0kfprjy60n696ckw866vwk2x43alylqn0yx3n6556w";
}) { config = {}; overlays = []; }
