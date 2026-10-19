library(tokenizers)

# clean cohort + normalize cohort names

{



  # check that cohort annotations are words
  # cohorts |>
  #   filter(cohort_annot != "") |>
  #   rowwise() |>
  #   mutate(cohort_annot_last_word = last(unlist(tokenize_words(cohort_annot, lowercase = F)))
  #   ) |>
  #   filter(!cohort_annot_last_word %in%
  #            stringr::str_remove_all(pattern = "'s|’s",
  #                                    unlist(tokenize_words(sentence, lowercase = F))
  #            )
  #   )



# remove brackets from annotations
cohorts <-
  cohorts |>
  # filter(grepl("\\[|\\(|\\]|\\)", cohort_annot)) |>
  mutate(cohort_annot = stringr::str_replace_all(cohort_annot,
                                                 pattern = "\\[",
                                                 replacement = " ; ") |>
                        stringr::str_remove_all(pattern = "\\((?![^()]*\\))") |>
                        stringr::str_remove_all("(?<!\\([^()]{0,50})\\)") |>
                        stringr::str_replace_all(pattern = "\\s+;",
                                                 replacement = ";"
                                                 ) |>
                       stringr::str_squish()

         ) |>
  tidyr::separate_longer_delim(cohort_annot,
                               delim = "; ") |>
  mutate(cohort_annot = stringr::str_squish(cohort_annot) |>
                        stringr::str_remove_all(pattern = "^-|-$|_$")
         )|>
  filter(stringr::str_length(cohort_annot) != 1) |>
  filter(!stringr::str_detect(pattern = "^\\d+$",
                             cohort_annot))

cohorts <-
  cohorts |>
  group_by(pubmed_id, sentence) |>
  summarise(cohort_annot = stringr::str_flatten(cohort_annot,
                                                collapse = "; ")) |>
  ungroup()

# ~ 2,860
cohorts |>
  tidyr::separate_longer_delim(cohort_annot, delim = "; ") |>
  distinct(cohort_annot) |>
  nrow()

}



{

source(here::here("code/normalize_cohort_names_fns.R"))

abbrev_map_sentences <-
cohorts |>
  filter(grepl(";", cohort_annot) & grepl("\\(", sentence)) |>
  rename(cohorts = cohort_annot) |>
  purrr::pmap(extract_abbrev_pairs_one) |>
  purrr::list_rbind() |>
  filter(!is.na(abbrev))

abbrev_map <-
abbrev_map_sentences |>
  rename(cohort_annot = abbrev) |>
  select(-sentence) |>
  distinct()

cohort_long_names <-
  cohorts |>
  filter(cohort_annot != "") |>
  tidyr::separate_longer_delim(cohort_annot, delim = "; ") |>
  select(-sentence) |>
  distinct()

cohort_long_names <-
left_join(
  cohort_long_names,
  abbrev_map,
by = c("pubmed_id", "cohort_annot")
) |>
  mutate(long = ifelse(is.na(long),
                       cohort_annot,
                       long
                       )
  )

# 2,570
cohort_long_names |>
  distinct(long) |>
  nrow()

}

cohort_names <- readxl::read_xlsx(here::here("data/cohort/cohort_desc_v3_Sep5.xlsx"),
                                    sheet = 1)

cohort_long_names |> pull(cohort_annot) %in% cohort_names$full_name |> sum()


cohort_long_names |> pull(long) %in% cohrt_names$full_name |> sum()

cohort_long_names |>
  filter(!long %in% cohort_names$full_name & !cohort_annot %in% cohrt_names$full_name)

{

cohort_long_names <- cohort_long_names |>
    tidyr::separate_longer_delim(cols = long, delim = "/")

cohort_long_names <-
  cohort_long_names |>
  dplyr::mutate(long_clean = clean_cohort_names(long))


# 2424
cohort_long_names |>
  distinct(long_clean) |>
  nrow()

}

{
version_info_string <-
c(
" Phase \\d+ Version \\d+$",
" Phase \\d+$",
" Version \\d+$",
" Stage \\d+$",
" Stage$",
" Study [1-5]$",
" \\d+ & \\d+$",
" [1-5]$",
" [1-5]*$",
" Freeze \\d+$",
" Freeze [A-Z]{1}$",
"\\bFREEZE[1-10]$",
" \\+$",
" Plus",
"\\+$",
" R\\d+$",
" [1-5]\\+$",
" R$",
" Supplementary$"
)

version_info_string <- paste0(version_info_string,
                              collapse = "|")

cohort_long_names <-
  cohort_long_names |>
  mutate(long_clean_version = stringr::str_extract(long_clean, pattern = version_info_string)) |>
  mutate(long_clean = stringr::str_remove_all(long_clean, pattern = version_info_string))


# 2350
cohort_long_names |>
  distinct(long_clean) |>
  nrow()

}

{

# create column of long form cohort names
cohort_long_names <-
cohort_long_names  |>
mutate(long_clean = remove_project_terms(long_clean))

# 2233
cohort_long_names |>
distinct(long_clean) |>
nrow()

}

{
  cohort_long_names <-
    cohort_long_names  |>
    mutate(long_clean =  consistent_transcriptions(long_clean) |>
                         consistent_plurals()
    )

  cohort_long_names |>
    distinct(long_clean) |>
    nrow()

}

cohorts |>
  filter(cohort_annot != "") |>
  distinct(pubmed_id, cohort_annot)

# differ by only capitalisation



# common misspellings / mistranscriptions /versions
# Genomic, Genome
# COVID, COVID 19
# Haemorrhagic -> Hemorrhagic S
# Analyses Analysis
# Africa America
# Environment  vs Environmental
# Register vs Registry
# Ischaemic -> Ischemic
# Anthropmetric -> Anthropometric

# For, In, Of

# plurals
# Trials
# Genetics
# Genomics
# Outcomes
# Orders
# Veterans
# Lipids
# Treatments


# shopt -s nullglob
# mkdir -p output/abbreviations
# out=output/abbreviations/paper_ab3p_abbreviations.tsv
# log=output/abbreviations/ab3p_stderr.log
#
# printf 'pmid\tshort_form\tlong_form\tprecision\n' > "$out"
# : > "$log"
#
# for txt_file in output/abstracts/[0-9]*.txt output/results_copy/[0-9]*.txt output/methods_copy/[0-9]*.txt; do
# filename=$(basename "$txt_file")
# if [[ "$filename" =~ ^([0-9]+) ]]; then
# pmid="${BASH_REMATCH[1]}"

# clean=$(mktemp /tmp/ab3p.XXXXXX)

# remove / replace characters that may cause problems with ab3p processing
# chunk text, so that text greater than ab3p's processing limit (1,000 characters)
# can be run
# iconv -f UTF-8 -t ASCII//TRANSLIT -c < "$txt_file" \
# | tr '_\140' "-'" \
# | tr '\n' ' ' | sed 's/  */ /g; s/ $//' \
# | awk -v max=3000 -f code/cohort_norm/chunk_txt_for_ab3p.awk > "$clean"
#
# apptainer run -B /tmp ab3p.sif "$clean" 2>>"$log" |
#   awk -F'|' -v pmid="$pmid" '
#             /^  / && NF == 3 {
#                 sf = $1; gsub(/^[ \t]+|[ \t]+$/, "", sf)
#                 lf = $2; gsub(/^[ \t]+|[ \t]+$/, "", lf)
#                 if ($3 + 0 > 0.9) print pmid "\t" sf "\t" lf "\t" $3 + 0
#             }' >> "$out"
#
# rm -f "$clean"
# else
#   echo "No leading PMID: $txt_file" >&2
# fi
# done

# # get only unique rows (i.e. unique abbreviations per pmid)
# # save into temp file first
# tmp=$(mktemp)
# head -n 1 "$out" > "$tmp"
# tail -n +2 "$out" | sort -u >> "$tmp"
# mv "$tmp" "$out"
#
# wc -l "$out" # ~22,000 unique abbreviations
#
# grep -c 'Failed to find sentence' "$log"  # ~16702

