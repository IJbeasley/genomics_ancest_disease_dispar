#

{
dbgap_info <- data.table::fread(here::here("output/gwas_cohorts/gwas_study_dbgap_cohort_names.csv"))

dbgap_info <- dbgap_info |> rename(pubmed_id = pmid)

dbgap_info  <-
  dbgap_info  |>
  tidyr::separate_longer_delim(cohort, delim = "|") |>
  mutate(dbgap_version = str_extract(dbgap_id,
                                     pattern = "\\.v[0-9]+\\.p[0-9]+$|\\.v[0-9]+$|\\.p[0-9]+$")) |>
  mutate(dbgap_version = str_remove_all(dbgap_version,
                                        pattern = "^\\.")) |>
  mutate(dbgap_id = str_remove_all(dbgap_id,
                                     pattern = "\\.v[0-9]+\\.p[0-9]+$|\\.v[0-9]+$")) |>
  select(-pmcid, -source) |>
  distinct()


dbgap_info  <-
  dbgap_info  |>
  group_by(pubmed_id, dbgap_id) |>
  summarise(dbgap_cohort = stringr::str_flatten(unique(cohort),
                                                collapse = "; ",
                                                na.rm = T),
            n_total = stringr::str_flatten(unique(n_total),
                                           collapse = "; ",
                                           na.rm = T),
            dbgap_version = stringr::str_flatten(unique(dbgap_version),
                                           collapse = "; ",
                                           na.rm = T)
            ) |>
  distinct()

dbgap_info  <-
  dbgap_info  |>
  tidyr::separate_longer_delim(dbgap_cohort, delim = "; ") |>
  rename(cohort_annot = dbgap_cohort)

# 130 unique dbgap ids
}

cohort_short_names <-
cohort_long_names |>
  mutate(
    short_name = ifelse(
      !stringr::str_detect(cohort_annot, "\\s"),
      cohort_annot,
      NA_character_
    )
  ) |>
  filter(!is.na(short_name)) |>
  distinct(pubmed_id, short_name)

update_cohort <-
left_join(
  cohort_short_names |>
  rename(cohort_annot = short_name) |>
    mutate(match = tolower(cohort_annot)),
  # group_by(pubmed_id) |>
  # summarise(cohort_annot = stringr::str_flatten(unique(cohort_annot),
  #                                               collapse = "; ",
  #                                               na.rm = T)),
dbgap_info |> mutate(match = tolower(cohort_annot)) |> select(-cohort_annot),
by =  c("pubmed_id", "match")
)

update_cohort  |> pull(dbgap_id) |> unique() |> length()


missed_matches <-
  anti_join(
    dbgap_info |> mutate(match = tolower(cohort_annot)) |> select(-cohort_annot),
    cohort_short_names |>
      rename(cohort_annot = short_name) |>
      mutate(match = tolower(cohort_annot)),
    # group_by(pubmed_id) |>
    # summarise(cohort_annot = stringr::str_flatten(unique(cohort_annot),
    #                                               collapse = "; ",
    #                                               na.rm = T)),
    by =  c("pubmed_id", "match")
  )


unique(missed_matches$pubmed_id)[unique(missed_matches$pubmed_id) %in% unique(cohort_short_names$pubmed_id)]

trial_names <-
data.table::fread(here::here("output/gwas_cohorts/gwas_study_clinical_trial_cohort_names.csv"))

trial_names <- trial_names |> rename(pubmed_id = pmid)

# known cohorts, due to trial name
trial_names  |>
  distinct(pubmed_id, cohort, full_name)

trial_names  |>
  distinct(pubmed_id, nct_id) |>
  group_by(nct_id) |>
  summarise(n = n()) |>
  arrange(desc(n))

left_join(
trial_names,
cohort_long_names,
by = "pubmed_id"
)
