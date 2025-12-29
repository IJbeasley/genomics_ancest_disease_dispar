# Quick investigations in hp / mondo conversions to UMLS CUIs
# and the format of UMLS data (MRCONSO.RRF)

# ontology terms to umls terms
hp <- fread(here::here("data/icd/hp_umls_mapping.csv"))
mondo <- fread(here::here("data/icd/mondo_umls_mapping.csv"))

unmapped_disease_terms <-
  disease_mapping |>
  filter(is.na(icd10_code)) |>
  pull(`DISEASE/TRAIT`) |>
  unique() |>
  tolower()

umls_cuis_icd10 <-
  umls_data |>
  filter(SAB %in% c("ICD10", "ICD10CM"))

# overlap with umls terms
disease_trait_umls  <-
  umls_cuis_icd10 |>
  filter(tolower(STR) %in% unmapped_disease_terms) |>
  mutate(`DISEASE/TRAIT` = tolower(STR)) |>
  select(`DISEASE/TRAIT`,
         icd10_code = CODE) |>
  group_by(`DISEASE/TRAIT`) |>
  summarise(icd10_code = paste(unique(icd10_code),
                               collapse = ", ")) |>
  distinct() |>
  mutate(icd10_code_origin = "UMLS term match")

disease_mapping <-
  disease_mapping |>
  mutate(`DISEASE/TRAIT` = tolower(`DISEASE/TRAIT`)) |>
  rows_patch(disease_trait_umls,
             by = "DISEASE/TRAIT")


unmapped_term <-
  disease_mapping |>
  filter(is.na(icd10_code)) |>
  pull(collected_all_disease_terms) |>
  unique()



# overlap with umls terms
collected_trait_umls  <-
  umls_cuis_icd10 |>
  filter(tolower(STR) %in% unmapped_term) |>
  mutate(collected_all_disease_terms = tolower(STR)) |>
  select(collected_all_disease_terms,
         icd10_code = CODE) |>
  group_by(collected_all_disease_terms) |>
  summarise(icd10_code = paste(unique(icd10_code),
                               collapse = ", ")) |>
  distinct() |>
  mutate(icd10_code_origin = "UMLS term match (collected term)")

disease_mapping <-
  disease_mapping |>
  rows_patch(collected_trait_umls,
             by = "collected_all_disease_terms")


