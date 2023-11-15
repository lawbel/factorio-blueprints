{
  description = ''
    Converts images into pixel-art blueprints for the game Factorio
  '';

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  # we rely on "haskell-flake" to do the heavy lifting for the nix config
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [haskell-flake.flakeModule];
      perSystem = { self', pkgs, ... }: {
        # this part needs specifying as the package name
        packages.default = self'.packages.factorio-blueprints;
        haskellProjects.default = {
          devShell.tools = hsPkgs: {
            inherit (hsPkgs) fourmolu;
          };
        };
      };
    };
}
