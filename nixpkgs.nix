# to update: $ nix-prefetch-url --unpack url
let
  fetchTarball = { url, sha256 }@attrs:
    if builtins.lessThan builtins.nixVersion "1.12"
    then builtins.fetchTarball { inherit url; }
    else builtins.fetchTarball attrs;
in import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/35832681f19e178ff5e4984c71e83ee0fbc7ebb3.tar.gz";
  sha256 = "12yfp52rcypqcngj5kh2jdxmld1bakh67qxhc16y7z7lgy0mj26n";
}) { config = {}; overlays = []; }
