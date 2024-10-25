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
        overlays = [ haskellNix.overlay ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        advent-of-code-project = pkgs.haskell-nix.project' {
          name = "advent-of-code";
          src = ./.;
          compiler-nix-name = "ghc982";
          shell = {
            withHoogle = false;
            buildInputs = [ format-haskell ];
            tools = {
              cabal = { };
              hlint = { };
              haskell-language-server = { };
              fourmolu = { };
            };
          };
        };
        inherit (advent-of-code-project.hsPkgs) advent-of-code;
        format-haskell =
          pkgs.writeShellApplication {
            name = "format-haskell";
            runtimeInputs = [ (advent-of-code-project.tool "fourmolu" { }) pkgs.haskellPackages.cabal-fmt ];
            text = ''
              # shellcheck disable=SC2046
              fourmolu --mode inplace $(git ls-files 'haskell/*.hs')
              cabal-fmt -i advent-of-code.cabal
            '';
          };
      in
      {
        packages = advent-of-code.components.exes //
          { default = advent-of-code.components.exes.aoc2023; };
        devShells.default = advent-of-code-project.shell;
        legacyPackages = pkgs // { inherit advent-of-code-project; };
        apps = {
          format =
            pkgs.writeShellApplication {
              name = "format-haskell";
              runtimeInputs = [ (advent-of-code-project.tool "fourmolu" { }) pkgs.haskellPackages.cabal-fmt ];
              text = ''
                # shellcheck disable=SC2046
                fourmolu --mode inplace $(git ls-files '**.hs')
                cabal-fmt -i advent-of-code.cabal
              '';
            };
        };
      }
    );
}
