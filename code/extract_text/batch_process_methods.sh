#!/bin/bash
# Batch process all XML files in a folder using extract_methods.py
# Usage: ./batch_process.sh input_folder output_folder

if [ $# -lt 2 ]; then
    echo "Usage: $0 <input_folder> <output_folder>"
    echo ""
    echo "Example:"
    echo "  $0 ./xml_files ./output_texts"
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
NO_METHODS=0
ONLINE_ONLY=0

# Arrays to store filenames
FAILED_FILES=()
NO_METHODS_FILES=()
ONLINE_ONLY_FILES=()

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

    # Output file name
    output_file="$OUTPUT_FOLDER/${filename}_methods.txt"

    echo -n "Processing: $(basename "$xml_file")... "

    # Run the extraction script
    if python3 "$SCRIPT_DIR/extract_methods.py" "$xml_file" -o "$output_file" 2>/dev/null; then
        if [ -s "$output_file" ]; then
            # Check if it's an online-only methods file
            if head -n 1 "$output_file" | grep -q "NOTE: Methods are only available online"; then
                echo "⚠ ONLINE ONLY"
                ONLINE_ONLY=$((ONLINE_ONLY + 1))
                ONLINE_ONLY_FILES+=("$(basename "$xml_file")")
            else
                echo "✓ SUCCESS"
                SUCCESS=$((SUCCESS + 1))
            fi
        else
            echo "⚠ NO METHODS SECTION"
            NO_METHODS=$((NO_METHODS + 1))
            NO_METHODS_FILES+=("$(basename "$xml_file")")
            rm -f "$output_file"  # Remove empty file
        fi
    else
        echo "✗ FAILED"
        FAILED=$((FAILED + 1))
        FAILED_FILES+=("$(basename "$xml_file")")
    fi
done

# Print summary
echo ""
echo "=================================================="
echo "SUMMARY"
echo "=================================================="
echo "Successfully processed: $SUCCESS"
echo "Online only:            $ONLINE_ONLY"
echo "No methods section:     $NO_METHODS"
echo "Failed:                 $FAILED"
echo "Total files:            $TOTAL"
echo ""
echo "Output files saved to: $OUTPUT_FOLDER"

# Save failed files list
if [ ${#FAILED_FILES[@]} -gt 0 ]; then
    FAILED_LIST="$OUTPUT_FOLDER/failed_files.txt"
    printf "%s\n" "${FAILED_FILES[@]}" >> "$FAILED_LIST"
    echo ""
    echo "Failed files list saved to: $FAILED_LIST"
    echo "Failed files:"
    for file in "${FAILED_FILES[@]}"; do
        echo "  - $file"
    done
fi

# Save no methods files list
if [ ${#NO_METHODS_FILES[@]} -gt 0 ]; then
    NO_METHODS_LIST="$OUTPUT_FOLDER/no_methods_files.txt"
    printf "%s\n" "${NO_METHODS_FILES[@]}" >> "$NO_METHODS_LIST"
    echo ""
    echo "No methods section list saved to: $NO_METHODS_LIST"
fi

# Save online-only files list
if [ ${#ONLINE_ONLY_FILES[@]} -gt 0 ]; then
    ONLINE_ONLY_LIST="$OUTPUT_FOLDER/online_only_methods_files.txt"
    printf "%s\n" "${ONLINE_ONLY_FILES[@]}" >> "$ONLINE_ONLY_LIST"
    echo ""
    echo "Online-only methods list saved to: $ONLINE_ONLY_LIST"
fi


#!/bin/bash
# Batch process all XML files in a folder using extract_methods.py
# Usage: ./batch_process.sh input_folder output_folder
#
# if [ $# -lt 2 ]; then
#     echo "Usage: $0 <input_folder> <output_folder>"
#     echo ""
#     echo "Example:"
#     echo "  $0 ./xml_files ./output_texts"
#     exit 1
# fi
#
# INPUT_FOLDER="$1"
# OUTPUT_FOLDER="$2"
#
# # Check if input folder exists
# if [ ! -d "$INPUT_FOLDER" ]; then
#     echo "Error: Input folder '$INPUT_FOLDER' does not exist"
#     exit 1
# fi
#
# # Create output folder if it doesn't exist
# mkdir -p "$OUTPUT_FOLDER"
#
# # Get the directory where this script is located
# SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
#
# # Count files
# TOTAL=0
# SUCCESS=0
# FAILED=0
# NO_METHODS=0
#
# # Arrays to store filenames
# FAILED_FILES=()
# NO_METHODS_FILES=()
#
# echo "Processing XML files in: $INPUT_FOLDER"
# echo "Output will be saved to: $OUTPUT_FOLDER"
# echo ""
#
# # Process each XML file
# for xml_file in "$INPUT_FOLDER"/*.xml; do
#     # Check if any XML files exist
#     if [ ! -e "$xml_file" ]; then
#         echo "No XML files found in $INPUT_FOLDER"
#         exit 1
#     fi
#
#     TOTAL=$((TOTAL + 1))
#
#     # Get filename without path and extension
#     filename=$(basename "$xml_file" .xml)
#
#     # Output file name
#     output_file="$OUTPUT_FOLDER/${filename}_methods.txt"
#
#     echo -n "Processing: $(basename "$xml_file")... "
#
#     # Run the extraction script
#     if python3 "$SCRIPT_DIR/extract_methods.py" "$xml_file" -o "$output_file" 2>/dev/null; then
#         if [ -s "$output_file" ]; then
#             echo "✓ SUCCESS"
#             SUCCESS=$((SUCCESS + 1))
#         else
#             echo "⚠ NO METHODS SECTION"
#             NO_METHODS=$((NO_METHODS + 1))
#             NO_METHODS_FILES+=("$(basename "$xml_file")")
#             rm -f "$output_file"  # Remove empty file
#         fi
#     else
#         echo "✗ FAILED"
#         FAILED=$((FAILED + 1))
#         FAILED_FILES+=("$(basename "$xml_file")")
#     fi
# done
#
# # Print summary
# echo ""
# echo "=================================================="
# echo "SUMMARY"
# echo "=================================================="
# echo "Successfully processed: $SUCCESS"
# echo "No methods section:     $NO_METHODS"
# echo "Failed:                 $FAILED"
# echo "Total files:            $TOTAL"
# echo ""
# echo "Output files saved to: $OUTPUT_FOLDER"
#
# # Save failed files list
# if [ ${#FAILED_FILES[@]} -gt 0 ]; then
#     FAILED_LIST="$OUTPUT_FOLDER/failed_files.txt"
#     printf "%s\n" "${FAILED_FILES[@]}" > "$FAILED_LIST"
#     echo ""
#     echo "Failed files list saved to: $FAILED_LIST"
#     echo "Failed files:"
#     for file in "${FAILED_FILES[@]}"; do
#         echo "  - $file"
#     done
# fi
#
# # Save no methods files list
# if [ ${#NO_METHODS_FILES[@]} -gt 0 ]; then
#     NO_METHODS_LIST="$OUTPUT_FOLDER/no_methods_files.txt"
#     printf "%s\n" "${NO_METHODS_FILES[@]}" > "$NO_METHODS_LIST"
#     echo ""
#     echo "No methods section list saved to: $NO_METHODS_LIST"
# fi
