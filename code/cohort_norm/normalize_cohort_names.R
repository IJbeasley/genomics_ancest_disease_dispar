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

