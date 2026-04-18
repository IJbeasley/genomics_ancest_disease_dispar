# check pubmedbert predictions on the validation set

predictions <- data.table::fread("output/pubmedbert_entity_predictions.csv")

predictions |> filter(true_cohorts != predicted_cohorts) |> View()

# standardize cohort names
cohort_group <-
  matched |>
  select(cohort, full_name, synonyms, group_id) |>
  tidyr::pivot_longer(cols = c(cohort, full_name, synonyms),
                      names_to = "field",
                      values_to = "COHORT") |>
  filter(!is.na(COHORT)) |>
  select(-field) |>
  distinct()

cohort_group <-
  cohort_group |>
  group_by(group_id) |>
  # group name is the shortest cohort name in the group (after trimming whitespace)
  mutate(group_name = COHORT[which.min(nchar(str_trim(COHORT)))][1]) |>
  ungroup() |>
  select(COHORT, group_name) |>
  #mutate(across(everything(), ~ tolower(.))) |>
  mutate(across(everything(), ~stringr::str_replace_all(., "\\s*-\\s*", "-"))) |>
  distinct()


# evaluate this per pubmed id:
true_cohorts_per_pubmed_id <-
predictions |>
  select(pubmed_id, true_cohorts) |>
  tidyr::separate_longer_delim("true_cohorts", delim = ";") |>
  mutate(true_cohorts = trimws(true_cohorts)) |>
  filter(true_cohorts != "")  |>
  mutate(true_cohorts = stringr::str_replace_all(true_cohorts, "\\s*-\\s*", "-"))


true_cohorts_per_pubmed_id <-
  left_join(true_cohorts_per_pubmed_id,
            cohort_group |> rename(true_cohorts = COHORT),
            by = "true_cohorts") |>
  mutate(group_name = ifelse(is.na(group_name), true_cohorts, group_name)) |>
  select(-true_cohorts) |>
  rename(true_cohorts = group_name)


true_cohorts_per_pubmed_id  <- true_cohorts_per_pubmed_id |>
  group_by(pubmed_id) |>
  summarise(true_cohorts = list(sort(unique(true_cohorts))))


predicted_cohorts_per_pubmed_id <-
predictions |>
  select(pubmed_id, predicted_cohorts) |>
  tidyr::separate_longer_delim("predicted_cohorts", delim = ";") |>
  mutate(predicted_cohorts = trimws(predicted_cohorts)) |>
  filter(predicted_cohorts != "")  |>
  mutate(predicted_cohorts = stringr::str_replace_all(predicted_cohorts, "\\s*-\\s*", "-"))

predicted_cohorts_per_pubmed_id <-
  left_join(predicted_cohorts_per_pubmed_id,
            cohort_group |> rename(predicted_cohorts = COHORT),
            by = "predicted_cohorts") |>
  mutate(group_name = ifelse(is.na(group_name), predicted_cohorts, group_name)) |>
  select(-predicted_cohorts) |>
  rename(predicted_cohorts = group_name)

predicted_cohorts_per_pubmed_id  <- predicted_cohorts_per_pubmed_id |>
  group_by(pubmed_id) |>
  summarise(predicted_cohorts = list(sort(unique(predicted_cohorts))))

# check recall, precision, and f1 score per pubmed id:
evaluation_per_pubmed_id <-
true_cohorts_per_pubmed_id |>
  left_join(predicted_cohorts_per_pubmed_id, by = "pubmed_id") |>
  rowwise() |>
  mutate(
    predicted_cohorts_syn = list(c(predicted_cohorts,
                                  paste(predicted_cohorts, "study", sep = " "),
                                  paste(predicted_cohorts, "project", sep = " "),
                                  paste(predicted_cohorts, "consortium", sep = " "),
                                  str_remove(predicted_cohorts, " study$"),
                                  str_remove(predicted_cohorts, " project$"),
                                  str_remove(predicted_cohorts, " consortium$"))
    )
    )|>
  mutate(
    true_positives = length(intersect(tolower(true_cohorts), tolower(predicted_cohorts_syn))),
    false_positives = length(setdiff(tolower(predicted_cohorts), tolower(predicted_cohorts_syn))),
    false_negatives = length(setdiff(tolower(true_cohorts), tolower(predicted_cohorts_syn))),
    precision = ifelse(true_positives + false_positives > 0, true_positives / (true_positives + false_positives), NA),
    recall = ifelse(true_positives + false_negatives > 0, true_positives / (true_positives + false_negatives), NA),
    f1_score = ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), NA)
  )
