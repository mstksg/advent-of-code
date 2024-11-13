{ advent-of-code, writeText, lib, writeTextDir, symlinkJoin }:
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

                    *[Prompt][d${daylong}p]* / *[Code][d${daylong}g]*

                    [d${daylong}p]: https://adventofcode.com/${year}/day/${dayshort}
                    [d${daylong}g]: https://github.com/${github}/advent-of-code/blob/master/${year}/AOC${daylong}/Day${daylong}.hs

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
          writeTextDir "Reflections-${year}.md"
            (builtins.concatStringsSep "\n\n" (lib.mapAttrsToList
              (d:
                dayout: builtins.readFile dayout)
              daysOut));
      in
      { inherit reflections; days = daysOut; }
    )
    (lib.filterAttrs (_: ym: builtins.isAttrs ym && builtins.hasAttr "days" ym) allDays);
  site = symlinkJoin {
    name = "advent-of-code-site";
    paths = [ ./static ] ++ lib.mapAttrsToList (_: y: y.reflections) renderedMap;
  };
in
{ inherit renderedMap site; }
