{ advent-of-code, writeText, lib, writeTextDir, symlinkJoin }:
let
  github = "mstksg";
  allDays =
    lib.filterAttrs
      (_: ym: builtins.isAttrs ym && builtins.hasAttr "days" ym)
      (
        lib.recursiveUpdate advent-of-code.reflections advent-of-code.benchmarks
      );
  renderedMap = builtins.mapAttrs
    (n: yearmap:
      let
        year = lib.removePrefix "aoc" n;
        daysOut =
          builtins.mapAttrs
            (d: daymap:
              let
                daylong = lib.removePrefix "day" d;
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
                    [d${daylong}g]: https://github.com/${github}/advent-of-code/blob/master/${year}/AOC${year}/Day${daylong}.hs

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
        reflectionsHeader =
          let
            yearLink = n2: _: if n2 == n then year else
            let y2 = lib.removePrefix "aoc" n2;
            in "[${y2}](https://github.com/${github}/advent-of-code/wiki/Reflections-${y2})";
            tocLink = d: _:
              let dshort = lib.removePrefix "0" (lib.removePrefix "day" d);
              in
              ''
                * [Day ${dshort}](https://github.com/${github}/advent-of-code/wiki/Reflections-${year}#day-${dshort})
              '';
          in
          ''
            ${builtins.concatStringsSep " / " (lib.mapAttrsToList yearLink allDays)}

            ${lib.concatStrings (lib.mapAttrsToList tocLink daysOut)}
          ''
        ;
        reflections =
          writeTextDir "Reflections-${year}.md"
            ''
              ${reflectionsHeader}
              ${builtins.concatStringsSep "\n\n" (lib.mapAttrsToList (d: dayout: builtins.readFile dayout) daysOut)}
            '';
      in
      { inherit reflections year; days = daysOut; }
    )
    allDays;
  home =
    let
      mkLink = n: ym:
        ''
          * [${ym.year}](https://github.com/${github}/advent-of-code/wiki/${lib.removeSuffix ".md" ym.reflections.name})
        ''
      ;
    in
    writeTextDir "Home.md"
      ''
        Check out the reflections page for each year!
      
        ${lib.concatStrings (lib.mapAttrsToList mkLink renderedMap)}
      ''

  ;
  site = symlinkJoin {
    name = "advent-of-code-site";
    paths = [ home ] ++ lib.mapAttrsToList (_: y: y.reflections) renderedMap;
  };
in
{ inherit renderedMap site; }
