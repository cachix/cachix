{
  description = "CLI for Hosted Nix binary caches";

  outputs = { self }: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" ];
    forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
  in {
    packages = forAllSystems (system: {
      cachix = let
        pkgs = import ./nix {
          inherit system;
        };
        cachix-api = pkgs.haskellPackages.callCabal2nix "cachix-api" (pkgs.gitignoreSource ./cachix-api) {};
        cachix = pkgs.haskellPackages.callCabal2nix "cachix" (pkgs.gitignoreSource ./cachix) { inherit cachix-api; };
      in pkgs.haskell.lib.disableLibraryProfiling cachix;
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.cachix);
  };
}
