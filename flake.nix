{
  description = ''
    Converts images into pixel-art blueprints for the game Factorio
  '';

  # we rely on "haskell-flake" to do the heavy lifting for the nix config
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" ];
    imports = [ inputs.haskell-flake.flakeModule ];

    perSystem = { self', pkgs, config, ... }: {
      packages.default = self'.packages.factorio-blueprints;

      haskellProjects.default = {
        basePackages = pkgs.haskell.packages.ghc947;
        devShell.tools = hsPkgs: {
          inherit (hsPkgs) fourmolu;
        };
        # don't auto-wire up devShells, as we manually specify them below
        autoWire = [ "packages" "apps" "checks" ];
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
      };
    };
  };
}
