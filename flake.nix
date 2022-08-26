{
  description = "Crypto Agda slides";

  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
    let
        agda-overlay = import ./agda-overlay.nix;
        overlays = [ agda-overlay ];
        pkgs = import nixpkgs { inherit system overlays; };
        inherit (pkgs) agdaPackagesNew;
        inherit (nixpkgs.lib) cleanSourceWith hasSuffix;
        agda-p = pkgs.agda.withPackages (with agdaPackagesNew; [ standard-library ]);
        latex = with pkgs; texlive.combine {
          inherit (texlive)
            scheme-full
          ;
        };
        name = "slides";
    in rec {
      packages = rec {
        slides = with pkgs;
          stdenv.mkDerivation {
              name = name;
              src = cleanSourceWith {
                filter = name: type:
                  !(hasSuffix ".nix" name)
                ;
                src = ./.;
              };
              buildInputs = [
                agda-p
                latex
              ];
            };
        default = slides;
      };
    });
}
