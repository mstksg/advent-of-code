{ advent-of-code, runCommand, lib, writeShellApplication, jq }:
let
  benchmarkMap =
    builtins.mapAttrs
      (n: exe:
        let
          year = builtins.substring 3 4 n;
          generate-benches = writeShellApplication {
            name = "generate-benches";
            runtimeInputs = [ exe jq ];
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
                  cat "$TEMPFILE" > "$OUTFILE"
                fi
              done
            '';
          };
        in
        generate-benches
      )
      advent-of-code.project.hsPkgs.advent-of-code.components.exes;
in
benchmarkMap
