{
  description = "CLI for Hosted Nix binary caches";

  outputs = { self }: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
  in {
    packages = forAllSystems (system: {
      cachix = import ./. { inherit system; };
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.cachix);
  };
}
