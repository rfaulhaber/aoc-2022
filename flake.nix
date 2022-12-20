{
  description = "Advent of Code 2022.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellDeps = with pkgs; [
          ghc
          haskellPackages.brittany
          haskellPackages.haskell-language-server
          cabal-install
        ];
        cdeps = with pkgs; [ clang gnumake lldb ];
        racketDeps = with pkgs; [ racket ];
      in {
        devShells.default =
          pkgs.mkShell { buildInputs = haskellDeps ++ cdeps ++ racketDeps; };

        templates = {
          haskell = {
            path = ./templates/haskell;
            description = "AOC Haskell template";
          };
          c = {
            path = ./templates/c;
            description = "AOC C template";
          };
          racket = {
            path = ./templates/racket;
            description = "AOC Racket template";
          };
        };
      });

}
