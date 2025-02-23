{ advent-of-code, runCommand, lib, writeShellApplication, jq, ansifilter, writeText }:
let
  benchmarkMap =
    builtins.mapAttrs
      (n: exe:
        let
          year = lib.removePrefix "aoc" n;
          bench-results = ./. + "/${year}";
          generate-benches = writeShellApplication {
            name = "generate-benches-${n}";
            runtimeInputs = [ exe jq ansifilter ];
            text = ''
              DIR="''${1:-./bench-results}"
              mkdir -p "$DIR/${year}"
              ${lib.getExe exe} list | jq -r '.[]' | while read -r DAY; do
                OUTFILE=$(printf "$DIR/${year}/day%02d.txt\n" "$DAY")
                if [ -f "$OUTFILE" ]; then
                  echo "$OUTFILE already exists, skipping ..."
                else
                  echo "Generating benchmark for $OUTFILE ..."
                  TEMPFILE=$(mktemp)
                  ${lib.getExe exe} bench "$DAY" | tee "$TEMPFILE"
                  ansifilter "$TEMPFILE" > "$OUTFILE"
                fi
              done
            '';
          };
        in
        {
          inherit generate-benches;
        } //
        lib.optionalAttrs (builtins.pathExists bench-results)
          {
            days = builtins.listToAttrs
              (builtins.map
                (fp:
                  let day = lib.removeSuffix ".txt" (builtins.baseNameOf fp);
                  in {
                    name = day;
                    value = {
                      benchmark = writeText "${n}-${day}-bench.txt" (builtins.readFile fp);
                    };
                  }
                )
                (lib.filesystem.listFilesRecursive bench-results));
          }
      )
      advent-of-code.project.hsPkgs.advent-of-code.components.exes;
in
benchmarkMap
