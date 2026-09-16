#!/bin/bash
# Batch process all XML files in a folder using extract_intro.py
# Usage: ./batch_process_intro.sh input_folder output_folder

if [ $# -lt 2 ]; then
    echo "Usage: $0 <input_folder> <output_folder>"
    echo ""
    echo "Example:"
    echo "  $0 output/fulltexts/europe_pmc output/intro"
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
TOO_SHORT=0
LEAD_FALLBACK=0

# Arrays to store filenames
FAILED_FILES=()
NO_SECTION_FILES=()
TOO_SHORT_FILES=()
LEAD_FALLBACK_FILES=()
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
    # predictable names like {pmid}_pdf_tei_intro.txt. Must match the
    # filename the Python extractor writes to.
    output_name="${filename/.pdf.tei/_pdf_tei}"

    # Output file name
    output_file="$OUTPUT_FOLDER/${output_name}_intro.txt"

    # extract_intro.py appends '_lead' to the stem when it had to fall back to
    # the leading body paragraphs (untitled introductions, Nature-style "Main"
    # articles, letters to the editor), so the actual file written may be
    # ${output_name}_intro_lead.txt. This is the introduction analogue of the
    # '_main' marker the methods and results extractors use -- named
    # differently because the introduction fallback takes only the opening
    # paragraphs, never the whole body.
    lead_file="$OUTPUT_FOLDER/${output_name}_intro_lead.txt"

    echo -n "Processing: $(basename "$xml_file")... "

    # Decide from what the extractor REPORTS, not from what is on disk.
    # Two input folders can map to the same output name (e.g. elsevier/ and
    # elsevier/elsevier_xml/ hold the same PMIDs), so a file left by an
    # earlier article -- or an earlier run -- would otherwise be read as this
    # article's success. The extractor prints "Introduction section extracted
    # to: <path>" on stdout only when it actually wrote a file, and explains on
    # stderr why it did not; it exits 1 for every "nothing to extract" case,
    # so the exit code alone cannot tell those apart from a real crash.
    run_output=$(python3 "$SCRIPT_DIR/extract_intro.py" "$xml_file" -o "$output_file" 2>&1)

    if echo "$run_output" | grep -q "Introduction section extracted to: .*_lead\.txt"; then
        echo "✓ SUCCESS (lead-paragraph fallback)"
        SUCCESS=$((SUCCESS + 1))
        LEAD_FALLBACK=$((LEAD_FALLBACK + 1))
        LEAD_FALLBACK_FILES+=("$(basename "$xml_file")")
    elif echo "$run_output" | grep -q "Introduction section extracted to:"; then
        echo "✓ SUCCESS"
        SUCCESS=$((SUCCESS + 1))
    elif echo "$run_output" | grep -q "too short to be a section"; then
        echo "⚠ TOO SHORT"
        TOO_SHORT=$((TOO_SHORT + 1))
        TOO_SHORT_FILES+=("$(basename "$xml_file")")
    elif echo "$run_output" | grep -q "No introduction section found"; then
        echo "⚠ NO INTRODUCTION SECTION"
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
echo "  of which lead-para:   $LEAD_FALLBACK"
echo "Too short:              $TOO_SHORT"
echo "No introduction:        $NO_SECTION"
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
    write_list "no_intro_files.txt" "No introduction section list" "${NO_SECTION_FILES[@]}"
fi

if [ ${#TOO_SHORT_FILES[@]} -gt 0 ]; then
    write_list "too_short_intro_files.txt" "Too-short introduction list" "${TOO_SHORT_FILES[@]}"
fi

# Lead-paragraph fallbacks are worth eyeballing: their end boundary is a cap
# rather than a real section boundary.
if [ ${#LEAD_FALLBACK_FILES[@]} -gt 0 ]; then
    write_list "lead_fallback_files.txt" "Lead-paragraph fallback list" "${LEAD_FALLBACK_FILES[@]}"
fi
