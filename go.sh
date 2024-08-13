#!/bin/bash
mkdir -p truncated
for SOURCE in *.mid; do
    echo "$SOURCE"
    SOURCE_CSV="${SOURCE%.mid}.csv"
    MODIFIED_CSV="${SOURCE%.mid}_truncated.csv"
    TARGET_MIDI="truncated/$SOURCE"
    midicsv "$SOURCE" "$SOURCE_CSV"
    dune exec truncate_df_midi "$SOURCE_CSV" > "$MODIFIED_CSV"
    # diff -u "$SOURCE_CSV" "$MODIFIED_CSV"
    csvmidi "$MODIFIED_CSV" "$TARGET_MIDI"
done
