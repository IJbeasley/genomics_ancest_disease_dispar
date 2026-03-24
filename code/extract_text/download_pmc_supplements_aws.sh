#!/usr/bin/env bash
# download_pmc_supplements_aws.sh
# Usage: bash download_pmc_supplements_aws.sh PMC1234567 PMC2345678 ...
#    or: bash download_pmc_supplements_aws.sh --file pmcids.txt

set -euo pipefail

BUCKET="s3://pmc-oa-opendata"
OUT_BASE="output/supplement"

# ── Parse arguments ────────────────────────────────────────────────────────────
pmcids=()
if [[ "${1:-}" == "--file" ]]; then
  while IFS= read -r line || [[ -n "$line" ]]; do
    [[ -z "$line" ]] && continue  # skip blank lines
    pmcids+=("$line")
  done < "$2"
# if [[ "${1:-}" == "--file" ]]; then
#   mapfile -t pmcids < "$2"
else
  pmcids=("$@")
fi

if [[ ${#pmcids[@]} -eq 0 ]]; then
  echo "Usage: $0 PMC1234567 PMC2345678 ..."
  echo "       $0 --file pmcids.txt"
  exit 1
fi

# ── Check aws cli is available ─────────────────────────────────────────────────
if ! command -v aws &>/dev/null; then
  echo "ERROR: aws CLI not found. Install from https://aws.amazon.com/cli/"
  exit 1
fi

# ── Process each PMCID ────────────────────────────────────────────────────────
for pmcid in "${pmcids[@]}"; do

  # Normalise: ensure uppercase PMC prefix
  pmcid=$(echo "$pmcid" | tr '[:lower:]' '[:upper:]')
  #pmcid="${pmcid^^}"
  out_dir="${OUT_BASE}/${pmcid}"

  # Skip if already downloaded
  if [[ -d "$out_dir" ]] && [[ -n "$(ls -A "$out_dir" 2>/dev/null)" ]]; then
    echo "[SKIP] ${pmcid} — already exists at ${out_dir}"
    continue
  fi

  echo "[INFO] Processing ${pmcid}..."

  # Step 1: Find all versions for this PMCID (e.g. PMC123.1/, PMC123.2/)
  versions=$(aws s3api list-objects-v2 \
    --bucket pmc-oa-opendata \
    --prefix "${pmcid}." \
    --delimiter "/" \
    --query "CommonPrefixes[].Prefix" \
    --output text \
    --no-sign-request 2>/dev/null || true)

  if [[ -z "$versions" || "$versions" == "None" ]]; then
    echo "[WARN] ${pmcid} — not found in PMC AWS bucket (may not be open access)"
    continue
  fi

  # Use the latest version (last in sorted list)
  latest_version=$(echo "$versions" | tr '\t' '\n' | sort | tail -1 | tr -d '/')
  echo "[INFO] Latest version: ${latest_version}"

  # Step 2: Fetch JSON metadata to get media_urls (supplements only)
  json_file=$(mktemp)
  aws s3 cp \
    "${BUCKET}/${latest_version}/${latest_version}.json" \
    "$json_file" \
    --no-sign-request \
    --quiet 2>/dev/null || { echo "[WARN] ${pmcid} — no JSON metadata found"; rm -f "$json_file"; continue; }

  # Step 3: Extract media_urls (supplementary + media files) from JSON
  # Excludes .xml, .txt, .pdf of the main article itself
  sup_urls=$(python3 -c "
import json, sys
data = json.load(open('$json_file'))
urls = data.get('media_urls', []) or []
for url in urls:
    # Strip md5 query param for aws s3 cp
    print(url.split('?')[0])
" 2>/dev/null)

  rm -f "$json_file"

  if [[ -z "$sup_urls" ]]; then
    echo "[INFO] ${pmcid} — no supplementary/media files available"
    continue
  fi

  mkdir -p "$out_dir"

  # Step 4: Download each supplementary file
  while IFS= read -r s3_url; do
    filename=$(basename "$s3_url")
    out_path="${out_dir}/${filename}"

    if [[ -f "$out_path" ]]; then
      echo "  [SKIP] ${filename} already downloaded"
      continue
    fi

    echo "  [DOWN] ${filename}"
    aws s3 cp "$s3_url" "$out_path" \
      --no-sign-request \
      --quiet \
      || echo "  [FAIL] Could not download ${filename}"

  done <<< "$sup_urls"

  n_files=$(find "$out_dir" -type f | wc -l)
  echo "[DONE] ${pmcid} — ${n_files} file(s) saved to ${out_dir}"

done
