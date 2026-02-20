#!/bin/bash
# for all txt files in output/fulltexts/methods_sections (all extracted methods text files)
# run python check script
# list files that fail checks
# Usage: bash batch_validate_methods.sh [output_directory]
#

# Configuration
OUTPUT_DIR="${1:-output/fulltexts/methods_sections}"
VALIDATOR="code/extract_text/check_methods_processing.py"
FAILED_LIST="failed_preprocessing_checks.txt"
PASSED_LIST="passed_preprocessing_checks.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Initialize counters
TOTAL=0
PASSED=0
FAILED=0

# Clear previous results
> "$FAILED_LIST"
> "$PASSED_LIST"

echo "=================================================="
echo "BATCH PREPROCESSING VALIDATION"
echo "=================================================="
echo "Directory: $OUTPUT_DIR"
echo "Validator: $VALIDATOR"
echo ""

# Check if validator exists
if [ ! -f "$VALIDATOR" ]; then
    echo -e "${RED}ERROR: Validator not found at $VALIDATOR${NC}"
    echo "Please ensure validate_preprocessing.py is in the correct location"
    exit 1
fi

# Check if directory exists
if [ ! -d "$OUTPUT_DIR" ]; then
    echo -e "${RED}ERROR: Directory not found: $OUTPUT_DIR${NC}"
    exit 1
fi

# Find all .txt files
FILES=("$OUTPUT_DIR"/*.txt)

if [ ! -e "${FILES[0]}" ]; then
    echo -e "${YELLOW}WARNING: No .txt files found in $OUTPUT_DIR${NC}"
    exit 0
fi

echo "Found ${#FILES[@]} files to validate"
echo "=================================================="
echo ""

# Process each file
for filepath in "${FILES[@]}"; do
    filename=$(basename "$filepath")
    TOTAL=$((TOTAL + 1))

    # Run validator (suppress output, just check exit code)
    if python3 "$VALIDATOR" "$filepath" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $filename"
        PASSED=$((PASSED + 1))
        echo "$filename" >> "$PASSED_LIST"
    else
        echo -e "${RED}✗${NC} $filename"
        FAILED=$((FAILED + 1))
        echo "$filename" >> "$FAILED_LIST"
    fi
done

echo ""
echo "=================================================="
echo "SUMMARY"
echo "=================================================="
echo "Total files:   $TOTAL"
echo -e "Passed:        ${GREEN}$PASSED${NC}"
echo -e "Failed:        ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -gt 0 ]; then
    echo -e "${YELLOW}Failed files saved to: $FAILED_LIST${NC}"
    echo ""
    echo "To see detailed errors for failed files:"
    echo "  while read file; do"
    echo "    echo \"Checking: \$file\""
    echo "    python3 $VALIDATOR \"$OUTPUT_DIR/\$file\""
    echo "  done < $FAILED_LIST"
    echo ""
    exit 1
else
    echo -e "${GREEN}All files passed validation!${NC}"
    rm -f "$FAILED_LIST"  # Clean up since nothing failed
    exit 0
fi

python3 code/extract_text/check_methods_processing.py
