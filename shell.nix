let
  deps = import ./npins;
  pkgs = import deps.nixpkgs { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.haskell.compiler.ghc96
    pkgs.haskell-language-server
  ];
}
