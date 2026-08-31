#!/bin/bash
# Batch process all XML files in a folder using extract_results.py
# Usage: ./batch_process_results.sh input_folder output_folder

if [ $# -lt 2 ]; then
    echo "Usage: $0 <input_folder> <output_folder>"
    echo ""
    echo "Example:"
    echo "  $0 output/fulltexts/europe_pmc output/fulltexts/results_sections"
    exit 1
fi

INPUT_FOLDER="$1"
OUTPUT_FOLDER="$2"

# Check if input folder exists
if [ ! -d "$INPUT_FOLDER" ]; then
    echo "Error: Input folder '$INPUT_FOLDER' does not exist"
    exit 1
fi

# Create output folder if it doesn't exist
mkdir -p "$OUTPUT_FOLDER"

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Count files
TOTAL=0
SUCCESS=0
FAILED=0
NO_SECTION=0
ONLINE_ONLY=0
SUPPLEMENTARY=0
MAIN_FALLBACK=0

# Arrays to store filenames
FAILED_FILES=()
NO_SECTION_FILES=()
ONLINE_ONLY_FILES=()
SUPPLEMENTARY_FILES=()
MAIN_FALLBACK_FILES=()
FAILED_REASONS=()

echo "Processing XML files in: $INPUT_FOLDER"
echo "Output will be saved to: $OUTPUT_FOLDER"
echo ""

# Process each XML file
for xml_file in "$INPUT_FOLDER"/*.xml; do
    # Check if any XML files exist
    if [ ! -e "$xml_file" ]; then
        echo "No XML files found in $INPUT_FOLDER"
        exit 1
    fi

    TOTAL=$((TOTAL + 1))

    # Get filename without path and extension
    filename=$(basename "$xml_file" .xml)

    # Normalise .pdf.tei suffix to _pdf_tei so GROBID output files get
    # predictable names like {pmid}_pdf_tei_results.txt. Must match the
    # filename the Python extractor writes to.
    output_name="${filename/.pdf.tei/_pdf_tei}"

    # Output file name
    output_file="$OUTPUT_FOLDER/${output_name}_results.txt"

    # extract_results.py appends '_main' to the stem when it had to fall back
    # to the whole <body> (Nature-style "Main" articles, letters to the
    # editor), so the actual file written may be ${output_name}_results_main.txt
    main_file="$OUTPUT_FOLDER/${output_name}_results_main.txt"

    echo -n "Processing: $(basename "$xml_file")... "

    # Decide from what the extractor REPORTS, not from what is on disk.
    # Two input folders can map to the same output name (e.g. elsevier/ and
    # elsevier/elsevier_xml/ hold the same PMIDs), so a file left by an
    # earlier article -- or an earlier run -- would otherwise be read as this
    # article's success. The extractor prints "Results section extracted to:
    # <path>" on stdout only when it actually wrote a file, and explains on
    # stderr why it did not; it exits 1 for every "nothing to extract" case,
    # so the exit code alone cannot tell those apart from a real crash.
    run_output=$(python3 "$SCRIPT_DIR/extract_results.py" "$xml_file" -o "$output_file" 2>&1)

    if echo "$run_output" | grep -q "Results section extracted to: .*_main\.txt"; then
        echo "✓ SUCCESS (whole-body fallback)"
        SUCCESS=$((SUCCESS + 1))
        MAIN_FALLBACK=$((MAIN_FALLBACK + 1))
        MAIN_FALLBACK_FILES+=("$(basename "$xml_file")")
    elif echo "$run_output" | grep -q "Results section extracted to:"; then
        echo "✓ SUCCESS"
        SUCCESS=$((SUCCESS + 1))
    elif echo "$run_output" | grep -q "only available online"; then
        echo "⚠ ONLINE ONLY"
        ONLINE_ONLY=$((ONLINE_ONLY + 1))
        ONLINE_ONLY_FILES+=("$(basename "$xml_file")")
    elif echo "$run_output" | grep -q "in supplementary materials"; then
        echo "⚠ IN SUPPLEMENT"
        SUPPLEMENTARY=$((SUPPLEMENTARY + 1))
        SUPPLEMENTARY_FILES+=("$(basename "$xml_file")")
    elif echo "$run_output" | grep -q "No results section found"; then
        echo "⚠ NO RESULTS SECTION"
        NO_SECTION=$((NO_SECTION + 1))
        NO_SECTION_FILES+=("$(basename "$xml_file")")
    else
        # Anything else is a real failure: a parse error, a traceback, a crash.
        # The message is kept so it shows up in the summary.
        echo "✗ FAILED"
        FAILED=$((FAILED + 1))
        FAILED_FILES+=("$(basename "$xml_file")")
        FAILED_REASONS+=("$(basename "$xml_file"): $(echo "$run_output" | tail -n 1)")
    fi
done

# Print summary
echo ""
echo "=================================================="
echo "SUMMARY"
echo "=================================================="
echo "Successfully processed: $SUCCESS"
echo "  of which whole-body:  $MAIN_FALLBACK"
echo "Online only:            $ONLINE_ONLY"
echo "In supplement:          $SUPPLEMENTARY"
echo "No results section:     $NO_SECTION"
echo "Failed (errors):        $FAILED"
echo "Total files:            $TOTAL"
echo ""
echo "Output files saved to: $OUTPUT_FOLDER"

# write_list <output-file-name> <label> <files...>
write_list() {
    local list_name="$1"; shift
    local label="$1"; shift
    local list_path="$OUTPUT_FOLDER/$list_name"
    local f
    for f in "$@"; do
        echo "$INPUT_FOLDER/$f" >> "$list_path"
    done
    echo ""
    echo "$label saved to: $list_path"
}

if [ ${#FAILED_FILES[@]} -gt 0 ]; then
    write_list "failed_files.txt" "Failed files list" "${FAILED_FILES[@]}"
    echo "Failed files:"
    for reason in "${FAILED_REASONS[@]}"; do
        echo "  - $reason"
    done
fi

if [ ${#NO_SECTION_FILES[@]} -gt 0 ]; then
    write_list "no_results_files.txt" "No results section list" "${NO_SECTION_FILES[@]}"
fi

if [ ${#ONLINE_ONLY_FILES[@]} -gt 0 ]; then
    write_list "online_only_files.txt" "Online-only results list" "${ONLINE_ONLY_FILES[@]}"
fi

if [ ${#SUPPLEMENTARY_FILES[@]} -gt 0 ]; then
    write_list "supplementary_results_files.txt" "Results-in-supplement list" "${SUPPLEMENTARY_FILES[@]}"
fi

# Whole-body fallbacks are worth eyeballing: they hold the full article body
# rather than a scoped results section.
if [ ${#MAIN_FALLBACK_FILES[@]} -gt 0 ]; then
    write_list "main_fallback_files.txt" "Whole-body fallback list" "${MAIN_FALLBACK_FILES[@]}"
fi
