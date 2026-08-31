

# ontology to trait

ukbb_ont_icd10 <- data.table::fread(here::here("data/icd/UK_Biobank_master_file.tsv"))

ukbb_ont_icd10 <-
  ukbb_ont_icd10 |>
  rename_with(~ tolower(gsub(" ", "_", .x))) |>
  filter(stringr::str_detect(`icd10_code/self_reported_trait_field_code`,
                             pattern = "[A-Z]{1}")) |>
  select(mapped_term_uri, icd10 = `icd10_code/self_reported_trait_field_code`) |>
  tidyr::separate_longer_delim(mapped_term_uri,
                               delim = ", ") |>
  tidyr::separate_longer_delim(
    mapped_term_uri,
    delim = stringr::regex("\\s*(\\|\\||\\||,)\\s*")
  ) |>
  distinct()


ukbb_ont_icd10 <-
  ukbb_ont_icd10 |>
  mutate(mapped_term_uri = stringr::str_replace_all(mapped_term_uri,
                                                    pattern = "_",
                                                    replacement = ":"))

# HP_0003011||HP_0000924||HP_0000707
# HP_0012378 | EFO_0009641
# EFO_0000668||HP_0000093

original_gwas_study_info <- data.table::fread(here::here("data/gwas_catalog/gwas-catalog-v1.0.3.1-studies-r2025-07-21.tsv"),
                                              sep = "\t",
                                              quote = "")


original_gwas_study_info <-
  original_gwas_study_info |>
  rename_with(~ tolower(gsub(" ", "_", .x)))

gwas_terms <-
original_gwas_study_info |>
  select(pubmed_id, mapped_trait_uri, mapped_background_trait_uri) |>
  distinct() |>
  tidyr::separate_longer_delim(cols = "mapped_trait_uri",
                               delim = ", "
                               )  |>
  tidyr::separate_longer_delim(cols = "mapped_background_trait_uri",
                               delim = ", "
                               )


gwas_terms <-
  gwas_terms |>
  mutate(all_trait_uri = case_when(mapped_trait_uri == "" & mapped_background_trait_uri != "" ~ mapped_background_trait_uri,
                                   mapped_trait_uri != "" & mapped_background_trait_uri == "" ~ mapped_trait_uri,
                                   mapped_trait_uri != "" & mapped_background_trait_uri != "" ~ paste(mapped_trait_uri, mapped_background_trait_uri, sep = ", "),
                                   TRUE ~ NA_character_)) |>
  tidyr::separate_longer_delim(cols = "all_trait_uri",
                               delim = ", "
                               ) |>
  select(pubmed_id, mapped_term_uri = all_trait_uri)



gwas_terms <-
  gwas_terms |>
  mutate(mapped_term_uri = stringr::str_extract(mapped_term_uri, "[^/]+$"))


mapped_terms <-
left_join(
  gwas_terms,
  ukbb_ont_icd10,
  by = "mapped_term_uri",
  relationship = "many-to-many"
)

mapped_terms <-
  mapped_terms |>
  mutate(mapped_term_uri = stringr::str_replace_all(mapped_term_uri,
                                                    pattern = "_",
                                                    replacement = ":"))

mapped_terms |>
  filter(is.na(icd10)) |>
  pull(mapped_term_uri) |>
  unique() |>
  length()

mapped_terms |>
  filter(!is.na(icd10)) |>
  pull(mapped_term_uri) |>
  unique() |>
  length()



manual_sssom <-
data.table::fread(here::here("data/icd/positive.sssom.tsv"),
                  skip = "subject_id")

# http://purl.obolibrary.org/obo/mondo/mappings/mondo_hasdbxref_icd10cm.sssom.tsv

gwas_study_info <- data.table::fread(here::here("output/gwas_cat/gwas_study_info_trait_cat.csv"))

disease_pmids <- gwas_study_info |> filter(DISEASE_STUDY == T) |> pull(PUBMED_ID) |> unique()

mapped_terms |>
  filter(is.na(icd10)) |>
  filter(pubmed_id %in% disease_pmids) |>
  pull(mapped_term_uri) |>
  unique() |>
  length()

mapped_terms |>
  filter(is.na(icd10)) |>
  filter(pubmed_id %in% disease_pmids) |>
  pull(mapped_term_uri) |>
  unique() -> unmapped

unmapped_efo <-
  mapped_terms |>
  filter(is.na(icd10)) |>
  filter(pubmed_id %in% disease_pmids) |>
  filter(grepl("EFO", mapped_term_uri)) |>
  pull(mapped_term_uri) |>
  unique()


unmapped_mondo <-
  mapped_terms |>
  filter(is.na(icd10)) |>
  filter(pubmed_id %in% disease_pmids) |>
  filter(grepl("MONDO", mapped_term_uri)) |>
  pull(mapped_term_uri) |>
  unique()

writeLines(unmapped_mondo, "unmapped_mondo.txt")


oxo_mappings_files <- list.files(here::here("data/icd/oxo"))

oxo_mappings <- purrr::map(oxo_mappings_files,
                           function(file_name) {

                             oxo_table <- data.table::fread(here::here(paste0("data/icd/oxo/",
                                                                              file_name)),
                                                            skip = "subject_id")

                           }
)

oxo_mappings <- purrr::list_rbind(oxo_mappings)

oxo_mappings <-
  oxo_mappings  |> rename(mapped_term_uri = subject_id,
                         icd10 = object_id) |>
  #filter(!grepl("ICD10CM", icd10)) |>
  mutate(icd10 = stringr::str_remove_all(icd10,
                                         pattern = "ICD10:|ICD10WHO:"))


gwas_terms <-
  gwas_terms |>
  mutate(mapped_term_uri = stringr::str_replace_all(mapped_term_uri,
                                                    pattern = "_",
                                                    replacement = ":"))


gwas_terms <-
  gwas_terms |>
  mutate(mapped_term_uri = toupper(mapped_term_uri))

oxo_maps <-
left_join(
gwas_terms,
oxo_mappings,
by = "mapped_term_uri",
relationship = "many-to-many"
)

oxo_maps <-
  left_join(
    oxo_maps,
    ukbb_ont_icd10 |> rename(ukbb_icd10 = icd10),
    by = "mapped_term_uri",
    relationship = "many-to-many"
  )

oxo_maps |>
  pull(mapped_term_uri) |>
  unique() |>
  length()


oxo_maps |>
  filter(is.na(icd10)) |>
  pull(mapped_term_uri) |>
  unique() |>
  length()

oxo_maps |>
  filter(is.na(icd10) & is.na(ukbb_icd10)) |>
  pull(mapped_term_uri) |>
  unique() |>
  length()

oxo_maps |>
  row_patch(ukbb_ont_icd10)


oxo_mappings$mapped_term_uri[1:10]

ukbb <-
stringr::str_replace_all(ukbb_ont_icd10$mapped_term_uri,
                         pattern = "_",
                         replacement = ":") |>
unique()

sum(ukbb %in% oxo_mappings$mapped_term_uri)


data.table::fread(here::here("data/icd/allXREFinDO.tsv"))



data.table::fread(here::here("data/icd/allXREFinDO.tsv")) -> do

do <-
  do |>
  mutate(icd10 = case_when(grepl("ICD10", xref) ~ xref,
                           TRUE ~ NA))

icd_doids <- do |> filter(!is.na(icd10)) |> pull(id)

do_terms <-
  do |>
  filter(id %in% icd_doids) |>
  tidyr::fill(icd10, .direction =  "down") |>
  mutate(xref = ifelse(grepl("ICD10", xref),
                            id,
                            xref)
         ) |>
  select(-id) |>
  rename(mapped_term_uri = xref)


do_terms  <-
  do_terms  |>
  rename(do_icd10 = icd10)


oxo_maps <-
  left_join(
    oxo_maps,
    do_terms,
    by = "mapped_term_uri",
    relationship = "many-to-many"
  )


oxo_maps |>
  filter(is.na(icd10) & is.na(ukbb_icd10) & is.na(do_icd10)) |>
  pull(mapped_term_uri) |>
  unique() |>
  length()

