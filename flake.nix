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
        haskellDeps = with pkgs; [ ghc haskellPackages.brittany cabal-install ];
        cdeps = with pkgs; [ clang ];
      in {
        devShells.default =
          pkgs.mkShell { buildInputs = haskellDeps ++ cdeps; };
      });

}
