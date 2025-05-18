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
      rec {
        defaultPackage = artiflakery;
        checks = {
          inherit artiflakery;
        };
      }
    ));
}
