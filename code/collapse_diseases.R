# Mapping gwas catalog diseases
# ensuring consistency, and collapse across studies

# All studies v1.0.3.1 - with added fields to match the unpublished downloads, including cohort identifiers and full summary statistics availability
gwas_study_info = data.table::fread("data/gwas_catalog/gwas-catalog-v1.0.3.1-studies-r2025-06-10.tsv",
                                    sep = "\t",
                                    quote = "")

gwas_study_info = gwas_study_info |>
  dplyr::rename_all(~gsub(" ", "_", .x))

# no non-numeric pubmed IDs
gwas_study_info |>
  dplyr::filter(!is.numeric(PUBMED_ID)) |>
  nrow()

gwas_study_info |>
  dplyr::group_by(PUBMED_ID) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(desc(n_studies)) |>
  dplyr::slice_head(n = 100) |>
  dplyr::mutate(PUBMED_ID = factor(as.character(PUBMED_ID), levels = as.character(PUBMED_ID))) |>
  ggplot(aes(x = PUBMED_ID, y = n_studies)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)
  )  # Optional: rotate x labels for readability +


gwas_study_info |>
  dplyr::group_by(PUBMED_ID) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(desc(n_studies)) |>
  dplyr::slice_head(n = 5)

# UKBB Cohort
gwas_study_info |>
  dplyr::filter(PUBMED_ID == "34662886")

# MVP Cohort
gwas_study_info |>
  dplyr::filter(PUBMED_ID == "39024449")


# UKBB
gwas_study_info$COHORT |> unique() |> length()

gwas_study_info |>
  dplyr::group_by(COHORT) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(desc(n_studies))


gwas_study_info |>
  dplyr::filter(COHORT == "UKBB" | COHORT == "UKB") |>
  nrow()

gwas_study_info |>
  dplyr::group_by(PUBMED_ID) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(desc(n_studies)) |>
  dplyr::slice_head(n = 100) |>
  dplyr::mutate(PUBMED_ID = as.character(PUBMED_ID)) |>
  ggplot(aes(x = PUBMED_ID, y = n_studies)) +
  geom_col()

gwas_study_info |>


gwas_study_info$JOURNAL |> unique()
#


gwas_study_info |> dplyr::pull(MAPPED_TRAIT) |> unique() |> length()



gwas_study_info |>
  dplyr::filter(MAPPED_TRAIT %in% unique_mapped_traits) |>
  dplyr::group_by(PUBMED_ID) |>
  dplyr::summarise(pubmed_with_unq_traits = dplyr::n()) |>
  dplyr::arrange(desc(pubmed_with_unq_traits))

gwas_study_info |>
  dplyr::filter(MAPPED_TRAIT %in% unique_mapped_traits) |>
  dplyr::group_by(MAPPED_BACKGROUND_TRAIT) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(desc(n_studies))

