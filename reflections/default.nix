{ lib, writeText, advent-of-code }:
let
  reflectionMap = builtins.foldl' (x: y: lib.recursiveUpdate x y) { }
    (builtins.map
      (fp:
        let
          path = lib.last (lib.splitString "reflections/" (builtins.toString fp));
          year = builtins.dirOf path;
          day = lib.removeSuffix ".md" (builtins.baseNameOf path);
        in
        lib.optionalAttrs (lib.hasSuffix ".md" path)
          {
            "aoc${year}".days = {
              ${day}.reflection = writeText "aoc${year}-${day}.md" (builtins.readFile fp);
            };
          }
      )
      (lib.filesystem.listFilesRecursive ./.));
in
reflectionMap
