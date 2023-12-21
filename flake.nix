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
              compiler-nix-name = "ghc963";
              shell = {
                withHoogle = false;
                tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.aoc2023.flake { };
      in
      flake
      // { packages.default = flake.packages."advent-of-code:exe:aoc2023"; }
    );
}
