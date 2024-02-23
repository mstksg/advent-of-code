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
            advent-of-code = final.haskell-nix.project' {
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
        flake = pkgs.advent-of-code.flake { };
      in
      flake
      // {
        packages =
          pkgs.advent-of-code.hsPkgs.advent-of-code.components.exes
          //
          {
            default = flake.packages."advent-of-code:exe:aoc2023";
          };
        legacyPackages = pkgs;
        apps.format = {
          type = "app";
          program =
            toString
              (pkgs.writeShellApplication {
                name = "formatHaskell.sh";
                runtimeInputs = [ flake.devShell ];
                text = ''
                  # shellcheck disable=SC2046
                  fourmolu --mode inplace $(git ls-files '*.hs')
                '';
              }) + "/bin/formatHaskell.sh";
        };
      }
    );
}
