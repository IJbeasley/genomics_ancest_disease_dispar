

abstracts = list.files(here::here("output/abstracts"))



gwas_study_info = gwas_study_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_study_info =
  gwas_study_info |>
  dplyr::filter(DISEASE_STUDY == T)

all_pmids = gwas_study_info$PUBMED_ID |> unique()

missing_abstracts = setdiff(all_pmids,
                             pmids_with_abstracts)



# these are papers without true abstracts - don't collect abstracts from them

test = readLines(here::here(paste0("output/fulltexts/", full_texts[1])))

db_gap_info = data.table::fread(here::here("output/gwas_cohorts/gwas_study_dbgap_accessions.csv"))

db_gap_study = db_gap_info |>
               filter(!(DBGAP_ID == "" & DBGAP_ACCESSION == "" ))

# ~400


gwas_study_info <- data.table::fread(here::here("output/gwas_cat/gwas_study_info_trait_group_l2.csv"))

gwas_study_info = gwas_study_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_cat_cohort_info = gwas_study_info |>
                       mutate(COHORT = stringr::str_remove_all(COHORT, "multiple|other")) |>
                       mutate(COHORT == ifelse(COHORT == "NR", "", COHORT)) |>
                       filter(COHORT != "")

pubmeds_with_cohort_info = unique(gwas_cat_cohort_info$PUBMED_ID)

# ~1,700

overlap = unique(db_gap_study$PUBMED_ID)[unique(db_gap_study$PUBMED_ID) %in% pubmeds_with_cohort_info]

length(overlap)

print(overlap)

# overlap with full dbgap info:
db_gap_study = db_gap_info |>
  filter(DBGAP_ACCESSION != "" )

overlap = unique(db_gap_study$PUBMED_ID)[unique(db_gap_study$PUBMED_ID) %in% pubmeds_with_cohort_info]

length(overlap)

print(overlap)


to_add =
gwas_study_info |>
  select(PUBMED_ID, COHORT) |>
  distinct()

table =
left_join(db_gap_info,
          to_add,
          by = "PUBMED_ID"
          )

table |>
  filter(PUBMED_ID == "29158497")

get_internal_dbgap_ids = function(term) {

  dbgap_links <- entrez_search(
    db = "gap",
    term = term
  )
}
