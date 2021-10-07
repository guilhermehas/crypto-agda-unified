{
  description = "Crypto Agda slides";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixos-21.url = github:NixOS/nixpkgs/nixos-21.05;
  };

  outputs = { self, flake-utils, nixos-21, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs { inherit system; };
        pkgs-21 = import nixos-21 { inherit system; };
        agda-p = pkgs.agda.withPackages (p: with p; [ standard-library ]);
        latex = with pkgs-21; texlive.combine {
          inherit (texlive)
            scheme-full
          ;
        };
        name = "slides";
    in rec {
      packages.${name} = with pkgs;
        stdenv.mkDerivation {
            name = name;
            src = ./.;
            buildInputs = [
              agda-p
              latex
            ];
          };
      defaultPackage = packages.${name};
    });
}
