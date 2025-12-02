{
  description = "Artiflakery";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:

    (flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        artiflakery = pkgs.haskellPackages.callPackage ./package.nix { };
      in
      {
        defaultPackage = artiflakery;
        packages = {
          inherit artiflakery;
        };
        checks = {
          inherit artiflakery;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            stack
            haskell.compiler.ghc96
            haskell-language-server
            nodejs
          ];
        };

      }
    ))
    // {
      nixosModules.default = import ./module.nix;
    };
}
