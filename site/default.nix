{ advent-of-code, writeText, lib }:
let
  github = "mstksg";
  allDays = lib.recursiveUpdate advent-of-code.reflections advent-of-code.benchmarks;
  allYears = builtins.filter (y: builtins.match "^[0-9]{4}$" y != null) (builtins.map (builtins.substring 3 4) (builtins.attrNames allDays));
  renderedMap = builtins.mapAttrs
    (n: yearmap:
      let
        year = builtins.substring 3 4 n;
        daysOut =
          builtins.mapAttrs
            (d: daymap:
              let
                daylong = builtins.substring 3 2 d;
                dayshort = lib.removePrefix "0" daylong;
                benchString =
                  ''
                    ### Day ${dayshort} Benchmarks

                    ```
                    ${lib.removeSuffix "\n\n" (builtins.readFile daymap.benchmark)}
                    ```
                  '';
                body =
                  ''
                    Day ${dayshort}
                    ------

                    <!--
                    This section is generated and compiled by the nix derivation at ./site/default.nix from the the file
                    `./reflections/${year}/day${daylong}.md`.  If you want to edit this, edit that file instead!
                    -->

                    *[Prompt][d${daylong}p]* / *[Code][d${daylong}g]* / *[Rendered][d${daylong}h]*

                    [d${daylong}p]: https://adventofcode.com/${year}/day/${dayshort}
                    [d${daylong}g]: https://github.com/${github}/advent-of-code/blob/master/${year}/AOC/Challenge/Day${daylong}.hs
                    [d${daylong}h]: https://${github}.github.io/advent-of-code/${year}/AOC.Challenge.Day${daylong}.html

                    ${lib.optionalString
                        (builtins.hasAttr "reflection" daymap)
                        (builtins.readFile daymap.reflection)
                    }

                    ${lib.optionalString (builtins.hasAttr "benchmark" daymap) benchString}
                  '';
              in
              writeText "${n}-${d}-rendered.md" body
            )
            yearmap.days;
        reflections =
          writeText "${n}-reflections.md"
          ''
            Reflections
            ===========

            ${builtins.concatStringsSep " / " allYears}

            ${builtins.concatStringsSep "\n\n" (lib.mapAttrsToList (d: dayout: builtins.readFile dayout) daysOut)}
          '';
      in
      { inherit reflections; days = daysOut; }
    )
    allDays;
in
renderedMap
