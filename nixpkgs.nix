# to update: $ nix-prefetch-url --unpack url
import (builtins.fetchTarball {
  url = "https://github.com/nixos/nixpkgs/archive/b88d66ce058b5f2894aa08ea4e97d3f35e134837.tar.gz";
  sha256 = "108yz2jamx29zijcxkj1s98816sj3khrp3wgxvpny7sf9izfky15";
}) { config = {}; overlays = []; }
