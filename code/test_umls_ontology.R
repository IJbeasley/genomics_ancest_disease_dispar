# Quick investigations in hp / mondo conversions to UMLS CUIs
# and the format of UMLS data (MRCONSO.RRF)


# OMIM
# ORPHANET
# HPO

# get UMLS CUIs with ICD10 mappings
umls_data <-
  data.table::fread(here::here("data/icd/2025AA/META/MRCONSO.RRF"),
                    sep = "|",
                    header = FALSE,
                    quote = "",
                    fill = TRUE,
                    na.strings = c("", "NA")
  )

colnames(umls_data)[1:18] <- c(
  "CUI","LAT","TS","LUI","STT","SUI","ISPREF",
  "AUI","SAUI","SCUI","SDUI","SAB","TTY","CODE",
  "STR","SRL","SUPPRESS","CVF"
)

umls_cuis_icd10 <-
  umls_data |>
  filter(SAB %in% c("ICD10")) |>
  pull(CUI) |>
  unique()

# create map between ICD-10 codes and OMMIM / ORPHANET / HPO terms
umls_mapping <-
  umls_data |>
  filter(CUI %in% umls_cuis_icd10) |>
  filter(SAB %in% c("ICD10", #"ICD10CM",
                    #"OMIM",
                    #"ORPHANET",
                    "HPO")) |>
  select(CUI,
         SOURCE = SAB,
         CODE,
         TERM = STR
  ) |>
  distinct()

umls_icd10_map <-
  umls_mapping |>
  filter(SOURCE == "ICD10") |>
  rename(icd10_description = TERM) |>
  rename(icd10_code = CODE) |>
  select(-SOURCE)

umls_ontology_map <-
  umls_mapping |>
  filter(SOURCE != "ICD10") |>
  rename(phenotype = TERM,
         hpo_term = CODE) |>
  select(-SOURCE)

umls_mapping <-
  left_join(
    umls_icd10_map,
    umls_ontology_map,
    by = c("CUI"),
    relationship = "many-to-many"
  ) |>
  select(-CUI) |>
  filter(hpo_term != "") |>
  distinct()

all_terms <- str_replace(all_terms, "_", ":")
hp_terms <- all_terms[grepl("^HP_", all_terms)]
hp_terms <- str_replace(hp_terms, "HP_", "HP:")

unmapped_studies <-
  disease_mapping |>
  filter(is.na(icd10_code)) |>
  pull(STUDY_ACCESSION) |>
  unique()


all_terms <- unique(c(gwas_study_info |>
                        filter(STUDY_ACCESSION %in% unmapped_studies) |>
                        pull(MAPPED_TRAIT_URI),
                      gwas_study_info |>
                        filter(STUDY_ACCESSION %in% unmapped_studies) |>
                        pull(MAPPED_TRAIT_URI)
)
)

all_terms <- all_terms[all_terms != ""]

all_terms <-str_extract_all(all_terms, pattern = "[A-Z]+_[0-9]+")
all_terms <- unlist(all_terms)
all_terms <- unique(all_terms)
# remove uberon terms
all_terms <- all_terms[!grepl("^UBERON_", all_terms)]
# remove HANCESTRO terms
all_terms <- all_terms[!grepl("^HANCESTRO_", all_terms)]

all_terms <- str_replace(all_terms, "_", ":")
all_terms <- sort(all_terms)
# writeLines(all_terms,
#            here::here("unmapped_study_terms.txt")
#
#            writeLines(hp_terms,
#                       here::here("hp_unmapped_study_terms.txt")
# )

# hp_terms <-all_terms[grepl("HP", all_terms)]
#
# writeLines(hp_terms,
#            here::here("hp_unmapped_study_terms.txt")
#
# mondo_terms <-all_terms[grepl("MONDO", all_terms)]
#
# writeLines(mondo_terms,
#            here::here("mondo_unmapped_study_terms.txt"))
#
# efo_terms <- all_terms[grepl("EFO", all_terms)]
#
# writeLines(efo_terms,
#            here::here("efo_unmapped_study_terms.txt")
# )



disease_mapping |>
  filter(icd10_code_origin == "Inferred from similar studies") |>
  group_by(collected_all_disease_terms) |> summarise(n = n()) |> View()

unmapped_studies <-
  disease_mapping |>
  filter(is.na(icd10_code)) |>
  pull(STUDY_ACCESSION) |>
  unique()

hp_terms <-
  gwas_study_info |>
  filter(STUDY_ACCESSION %in% unmapped_studies) |>
  filter(str_count(MAPPED_TRAIT_URI, "http") == 1) |>
  mutate(hp_term = str_extract(MAPPED_TRAIT_URI, "HP_[0-9]{7}")) |>
  filter(!is.na(hp_term)) |>
  pull(hp_term) |>
  unique()

efo_terms <-
  gwas_study_info |>
  filter(STUDY_ACCESSION %in% unmapped_studies) |>
  filter(str_count(MAPPED_TRAIT_URI, "http") == 1) |>
  mutate(efo_term = str_extract(MAPPED_TRAIT_URI, "EFO_[0-9]{7}")) |>
  filter(!is.na(efo_term)) |>
  pull(efo_term) |>
  unique()

get_uk_mapping <-
  gwas_study_info |>
  filter(STUDY_ACCESSION %in% unmapped_studies) |>
  filter(str_count(MAPPED_TRAIT_URI, "http") == 1) |>
  mutate(term = str_extract(MAPPED_TRAIT_URI, "HP_[0-9]{7}|EFO_[0-9]{7}")) |>
  filter(!is.na(term)) |>
  filter(term %in% uk_matching$MAPPED_TERM_URI) |>
  select(STUDY_ACCESSION,
         MAPPED_TERM_URI  = term)

get_uk_mapping <-
  left_join(
    get_uk_mapping,
    uk_matching,
    by = c("MAPPED_TERM_URI"),
    relationship = "many-to-one"
  )

fread(here::here("data/icd/UK_Biobank_master_file.tsv")) -> uk_test

uk_matching <-
  uk_test |>
  filter(MAPPING_TYPE == "Exact") |>
  tidyr::separate_longer_delim(MAPPED_TERM_URI, delim = "|") |>
  filter(!grepl(",", MAPPED_TERM_URI)) |>
  filter(grepl("[A-Z][0-9]", `ICD10_CODE/SELF_REPORTED_TRAIT_FIELD_CODE`))

uk_matching <-
  uk_matching |>
  filter(MAPPED_TERM_URI %in% hp_terms | MAPPED_TERM_URI %in% efo_terms) |>
  select(MAPPED_TERM_URI,
         icd10_code = `ICD10_CODE/SELF_REPORTED_TRAIT_FIELD_CODE`
  )

# summarise by MAPPED_TERM_URI to have all icd10 codes in one row
uk_matching <- uk_matching |>
  group_by(MAPPED_TERM_URI) |>
  summarise(icd10_code = paste(unique(icd10_code), collapse = ", "))


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


