# to update: $ nix-prefetch-url --unpack url
import (builtins.fetchTarball {
  url = "https://github.com/nixos/nixpkgs/archive/338b407b27df2be760690c52fc0f13de0312504d.tar.gz";
  sha256 = "1haf6rhrc9apzbkfmxqpqm6a6l3y85j9rfr44pwsazjpafm0pj7m";
}) { config = {}; overlays = []; }
