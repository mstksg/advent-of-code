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
          (final: prev:
            {
              advent-of-code = {
                reflections = prev.callPackage ./reflections { };
                benchmarks = prev.callPackage ./bench-results { };
                site = prev.callPackage ./site { };
                project = prev.haskell-nix.project'
                  {
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
              };
            }
          )
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        advent-of-code-project = pkgs.advent-of-code.project;
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
          {
            inherit (pkgs.advent-of-code.site) site;
            default = advent-of-code.components.exes.aoc2023;
          };
        devShells.default = advent-of-code-project.shell;
        legacyPackages = pkgs;
        apps =
          (pkgs.lib.concatMapAttrs
            (n: bs: {
              "generate-benches-${n}" = {
                type = "app";
                program = pkgs.lib.getExe bs.generate-benches;
              };
            })
            pkgs.advent-of-code.benchmarks)
          //
          {
            format =
              let
                app = pkgs.writeShellApplication {
                  name = "format-haskell";
                  runtimeInputs = [ (advent-of-code-project.tool "fourmolu" { }) pkgs.haskellPackages.cabal-fmt ];
                  text = ''
                    # shellcheck disable=SC2046
                    fourmolu --mode inplace $(git ls-files '**.hs')
                    cabal-fmt -i advent-of-code.cabal
                  '';
                };
              in
              { type = "app"; program = pkgs.lib.getExe app; };
          };
      }
    );
}
