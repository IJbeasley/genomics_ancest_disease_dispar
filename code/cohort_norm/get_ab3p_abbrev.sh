shopt -s nullglob
mkdir -p output/abbreviations
out=output/abbreviations/paper_ab3p_abbreviations.tsv
log=output/abbreviations/ab3p_stderr.log
batch=$(mktemp /tmp/ab3p_batch.XXXXXX)
manifest=$(mktemp /tmp/ab3p_man.XXXXXX)

printf 'pmid\tshort_form\tlong_form\tprecision\n' > "$out"
: > "$log"

# Phase 1: normalise and chunk text.
for txt_file in output/abstracts/[0-9]*.txt \
                output/results_copy/[0-9]*.txt \
                output/methods_copy/[0-9]*.txt; do

    filename=$(basename "$txt_file")

    if [[ "$filename" =~ ^([0-9]+) ]]; then
        pmid="${BASH_REMATCH[1]}"

        # Convert UTF-8 to ASCII
        # Replace _ and ` with - and '
        # Join all lines
        # Collapse whitespace
        # Split into <=3000-character chunks
        # Append chunks to $batch
        n=$(iconv -f UTF-8 -t ASCII//TRANSLIT -c < "$txt_file" \
            | tr '_\140' "-'" \
            | tr '\n' ' ' \
            | sed 's/  */ /g; s/ $//' \
            | awk -v max=3000 -f code/cohort_norm/chunk_txt_for_ab3p.awk \
            | tee -a "$batch" \
            | wc -l)

printf '%s\n' "$pmid" | awk -v n="$n" '{ for (i = 1; i <= n; i++) print }' >> "$manifest"
    else
        echo "No leading PMID: $txt_file" >&2
    fi
done

echo "batched $(wc -l < "$batch") chunk(s) from $(sort -u "$manifest" | wc -l) document(s)" >&2

# Phase 2: one container start for the entire corpus.
apptainer run -B /tmp ab3p.sif "$batch" 2>"$log" \
    | awk -v manifest="$manifest" -v minprec=0.9 -f code/cohort_norm/join.awk >> "$out"

rm -f "$batch" "$manifest"
