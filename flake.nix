{
  description = "aoc2023";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            aoc2023 = final.haskell-nix.project' {
              name = "advent-of-code";
              src = ./.;
              compiler-nix-name = "ghc964";
              shell = {
                withHoogle = false;
                tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                  fourmolu = { };
                };
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.aoc2023.flake { };
      in
      flake
      // {
        packages = rec {
          aoc2020 = flake.packages."advent-of-code:exe:aoc2020";
          aoc2023 = flake.packages."advent-of-code:exe:aoc2023";
          default = aoc2023;
        };
      }
    );
}
