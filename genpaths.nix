# Realizes <num>> of derivations with size of <size>MB
# Useful for upload debugging
{
  size ? 1, # MB
  num ? 10, # count
  currentTime ? builtins.currentTime,
}:

with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz") { });

let
  drv =
    i:
    runCommand "${toString currentTime}-${toString i}" { } ''
      dd if=/dev/random of=$out bs=1MB count=${toString size}
    '';
in
writeText "empty-${toString num}-${toString size}MB" ''
  ${lib.concatMapStringsSep "" drv (lib.range 1 num)}
''
