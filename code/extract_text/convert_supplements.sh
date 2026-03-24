#!/bin/bash

INPUT_LIST="output/supplement/supplemental_files_to_convert.txt"
JAR="FormatConverter.jar"
SKIPPED_LOG="output/supplement/skipped_files.txt"
FAILED_LOG="output/supplement/failed_files.txt"

# Clear logs from previous runs
> "$SKIPPED_LOG"
> "$FAILED_LOG"

if [ ! -f "$INPUT_LIST" ]; then
    echo "Error: $INPUT_LIST not found"
    exit 1
fi

if [ ! -f "$JAR" ]; then
    echo "Error: $JAR not found in current directory"
    exit 1
fi

success=0
skipped=0
failed=0

# Read from fd 3 so java cannot accidentally consume stdin
while IFS= read -r input_file <&3 || [ -n "$input_file" ]; do

    # Skip empty lines and comments
    [[ -z "$input_file" || "$input_file" == \#* ]] && continue

    # Get file extension (lowercase)
    ext=$(echo "${input_file##*.}" | tr '[:upper:]' '[:lower:]')

    # Determine format argument based on extension
    case "$ext" in
        pdf)
            format="PDF"
            ;;
        docx|doc)
            format="MSWord"
            ;;
        xlsx|xls)
            format="MSExcel"
            ;;
        txt)
            format="FreeText"
            ;;
        *)
            echo "SKIPPING (unsupported format .$ext): $input_file"
            echo "$input_file" >> "$SKIPPED_LOG"
            ((skipped++))
            continue
            ;;
    esac

    # Derive output path: replace extension with .xml
    output_file="${input_file%.*}.xml"

    echo "Converting [$format]: $input_file -> $output_file"

    java -jar "$JAR" "$input_file" "$output_file" BioC "$format" 0 2>/dev/null

    if [ $? -ne 0 ]; then
        echo "  WARNING: Failed to convert $input_file" >&2
        echo "$input_file" >> "$FAILED_LOG"
        ((failed++))
    else
        ((success++))
    fi

done 3< "$INPUT_LIST"

echo ""
echo "========================================"
echo "Done."
echo "  Converted successfully : $success"
echo "  Skipped (unsupported)  : $skipped  (see $SKIPPED_LOG)"
echo "  Failed                 : $failed   (see $FAILED_LOG)"
echo "========================================"
