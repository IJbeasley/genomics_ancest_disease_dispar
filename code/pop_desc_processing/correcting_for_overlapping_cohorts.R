# addressing sample overlap
# assign number of samples to cohort ...

####### Get cohort groups ##########
{


library(stringdist)



cohort_group <-
  cohort_group |>
  mutate(COHORT = clean_cohort(COHORT),
         group_name = clean_cohort(group_name))

}




####### IDs extracted from text etc. ###########
{








]




}


########## Manual doccano annotations ############
{


}


############ Applied text-mining model annotations #############

# pubmedbert_cohort_annotations |>
#   filter(grepl("imputation", sentence, ignore.case = TRUE))
# remove versioning etc.




# remove freeze at the end of some annotations










# low confidence cohorts
# found in 1 sentence, and not in cohort_names
single_sentence_annotations <-
  pubmedbert_cohorts |>
  select(cohort_annotations, sentence) |>
  group_by(cohort_annotations) |>
  summarise(n_sentences = n()) |>
  filter(n_sentences == 1) |>
  pull(cohort_annotations)

low_confidence <- single_sentence_annotations[!c(single_sentence_annotations %in% cohort_group$COHORT)]

possible_matches <-
  fuzzy_match(low_confidence,
              cohort_group$COHORT,
              threshold = 2)

data.frame(low_confidence, possible_matches) |>
  filter(possible_matches != "") |>
  View()

pubmedbert_cohorts |>
  filter(cohort_annotations %in% low_confidence) |>
  select(cohort_annotations, sentence) |>
  rowwise() |>
  filter(grepl(
    sentence,
    pattern = paste0("\\b\\Q", cohort_annotations, "\\E\\b\\s(array|Array)\\b"),
    perl = TRUE
  ))

pubmedbert_cohorts <-
  pubmedbert_cohorts |>
  group_by(PUBMED_ID) |>
  summarise(pubmedbert_cohorts = str_flatten(unique(cohort_annotations),
                                              collapse = "|",
                                              na.rm = TRUE))




############ GWAS Catalog annotations ################



############### Combined all sources of cohort information ###############




all_cohort_info |>
  filter(pubmedbert_cohorts != "" | !is.na(pubmedbert_cohorts)) |>
  filter(gwas_cat_cohort != "" & !is.na(gwas_cat_cohort)) |>
  select(PUBMED_ID, pubmedbert_cohorts, gwas_cat_cohort)


gwas_study_info_cohort = left_join(gwas_study_info_cohort,
                    annotated_pubmeds |>
                      select(pubmed_id, cohort_annotations),
                    by = c("PUBMED_ID" = "pubmed_id")) |>
  select(-COHORT) |>
  rename(COHORT = cohort_annotations)


dbgap_cohort_names <- data.table::fread(
                   here::here("output/gwas_cohorts/gwas_study_combined_dbgap_pubmed_mapping.csv")
)

# for 39621102, max: 6458
# for 34662886, 454,787
# for 34737426, 456,348
# for 31832568,  244 individuals
# for 29915124, sum (634)
# for 39024449, 635,969
# for 30104761, 408,961
# for 38185688 331,522
# for 36180795 110,182 + 1,503,898
# for 32514122, 212,453

gwas_study_info =
  gwas_study_info |>
  rename_all(~gsub(" ", "_", .x))

gwas_study_info <-
gwas_study_info |>
  filter(PUBMED_ID %in% all_pmids) |>
  select(STUDY, PUBMED_ID) |>
  distinct()



{
  participant_words <- c("adults", "adolescents", "children", "infants",
                        "neonates",
                        "women",
                        "men", "female", "male","trios", "families", "twins", "siblings",
                        "cases", "probands", #"case subjects", #"([A-Za-z-]+)\\scases", ,
                        "controls", #"control subjects",   #"([A-Za-z-]+)\\scontrols"
                        "individuals", "patients",  "participants",
                        "subjects", "samples", "donors")
  #“([A-Za-z-]+)\\sparticipants”
  #"\\b[0-9,]+\\s+([A-Za-z-]+)\\scases"

  # “population controls”,
  # "affected individuals"
  # "study participants"
  participant_phrases_regex <- paste0("\\b[0-9,]+\\s+([A-Za-z-]+)\\s", "(",
                                      paste0(participant_words, collapse = "|"),
                                      ")")

  participant_phrases_regex_v2 <- paste0("\\b[0-9,]+\\s+([A-Za-z-]+)\\s+([A-Za-z-]+)\\s", "(",
                                         paste0(participant_words, collapse = "|"),
                                         ")")

  participant_phrases_regex_v3 <- paste0("\\b[0-9,]+\\s+([A-Za-z-]+)\\s+([A-Za-z-]+)\\s([A-Za-z-]+)\\s", "(",
                                         paste0(participant_words, collapse = "|"),
                                         ")")

  participant_regex <- paste0("\\b[0-9,]+\\s+", "(",
                              paste0(participant_words, collapse = "|"),
                              ")")


  n_participants_regex <- "(n|N|Neffective|Neff|Ncontrol|Ncase|ncase|ncontrol|ntotal)\\s*=\\s*[0-9,]"}


}

gwas_study_info <- fread(here::here("data/gwas_catalog/gwas-catalog-v1.0.3.1-studies-r2025-07-21.tsv"))


gwas_study_info |>
  filter(str_detect(STUDY, participant_phrases_regex) |
           str_detect(STUDY, participant_regex) |
           str_detect(STUDY, participant_phrases_regex_v2) |
           str_detect(STUDY, participant_phrases_regex_v3) |
           str_detect(STUDY, n_participants_regex)) |>
  select(STUDY) |>
  distinct() |>
  pull()




# gwas_sample_size <-
# gwas_ancest_info |>
#   filter(PUBMED_ID %in% training_sample) |>
#   group_by(PUBMED_ID, STUDY_ACCESSION) |>
#   summarise(n_sample = sum(NUMBER_OF_INDIVIDUALS)) |>
#   group_by(PUBMED_ID) |>
#   summarise(n = n(),
#             n_sample = str_flatten(unique(n_sample), collapse = ", ")
#   ) |>
#   filter(grepl(",",
#                n_sample))




gwas_ancest_subset |>
  filter(str_detect(BROAD_ANCESTRAL_CATEGORY,
                    "(?<!Eastern), ")) |>
  pull(BROAD_ANCESTRAL_CATEGORY) |>
  unique()

gwas_ancest_subset |>
  filter(BROAD_ANCESTRAL_CATEGORY == "NR, Other")

# remove NR, Othr group
gwas_ancest_subset <-
gwas_ancest_subset |>
  mutate(BROAD_ANCESTRAL_CATEGORY =
           ifelse(BROAD_ANCESTRAL_CATEGORY ==  "NR, Other",
                  "NR",
                  BROAD_ANCESTRAL_CATEGORY)
  )





# individual dataset bugs / differences correction
{











}

s{
}




# 169579
gwas_ancest_info |>
  filter(STAGE == "initial") |>
  nrow()

# 169579
gwas_ancest_info |>
  filter(STAGE == "initial") |>
   select(STUDY_ACCESSION,
         NUMBER_OF_INDIVIDUALS,
         BROAD_ANCESTRAL_CATEGORY) |>
  distinct() |>
  nrow()

# 169537
gwas_ancest_info |>
  filter(STAGE == "initial") |>
  select(STUDY_ACCESSION,
         NUMBER_OF_INDIVIDUALS) |>
  distinct() |>
  nrow()

gwas_ancest_info |>
  filter(STAGE == "initial") |>
  group_by(STUDY_ACCESSION,
           BROAD_ANCESTRAL_CATEGORY)  |>
  summarise(n = n()) |>
  filter(n > 1)

# 169170
gwas_ancest_info |>
  filter(STAGE == "initial") |>
  select(STUDY_ACCESSION,
         BROAD_ANCESTRAL_CATEGORY) |>
  distinct() |>
  nrow()

gwas_ancest_info |>
  filter(STAGE == "initial") |>
  group_by(STUDY_ACCESSION,
         NUMBER_OF_INDIVIDUALS)  |>
  summarise(n = n()) |>
  filter(n > 1)

gwas_ancest_info |>
  filter(STAGE == "initial",
         STUDY_ACCESSION == "GCST000924",
         NUMBER_OF_INDIVIDUALS == 60)


# 200401
gwas_ancest_info |>
  select(STUDY_ACCESSION,
         NUMBER_OF_INDIVIDUALS) |>
  distinct() |>
  nrow()

# where sample description is trios
# sibling pair
separate_gwas_n_samples |>
  filter(str_detect(sample_description,
                    "pair"))

separate_gwas_n_samples |>
  filter(str_detect(sample_description,
                    "trios"))

separate_gwas_n_samples <-
separate_gwas_n_samples |>
  mutate(n = ifelse(str_detect(sample_description,
                               "pair") & 2 * n == NUMBER_OF_INDIVIDUALS,
                   n = 2 * NUMBER_OF_INDIVIDUALS,
                   NUMBER_OF_INDIVIDUALS)) |>
  mutate(n = ifelse(str_detect(sample_description,
                               "trios") & 3 * n == NUMBER_OF_INDIVIDUALS,
                    n = 3 * NUMBER_OF_INDIVIDUALS,
                    NUMBER_OF_INDIVIDUALS))




# where there are multiple broad ancestral categories
# separate into multiple rows
broad_ancestry_sample_desc  <-
  broad_ancestry_sample_desc |>
  mutate(BROAD_ANCESTRAL_CATEGORY_ORIG = BROAD_ANCESTRAL_CATEGORY) |>
  tidyr::separate_longer_delim(BROAD_ANCESTRAL_CATEGORY,
                               delim = stringr::regex("(?<!Eastern), "))

# group broad ancestry group by sample description
# broad_ancestry_sample_desc <-
  broad_ancestry_sample_desc |>
  group_by(PUBMED_ID,
           STUDY_ACCESSION,
           BROAD_ANCESTRAL_CATEGORY,
           NUMBER_OF_INDIVIDUALS,
           sample_description,
           n
           ) |>
  summarise(n = n()) |>
  filter(n > 1)

broad_ancestry_sample_desc |>
  filter(STUDY_ACCESSION == "GCST000940",
         BROAD_ANCESTRAL_CATEGORY == "African American or Afro-Caribbean")

broad_ancestry_sample_desc |>
  group_by()
  filter(STUDY_ACCESSION == "GCST012115")

  summed_sample_desc <-
  broad_ancestry_sample_desc |>
  filter(sample_broad_ancestry == BROAD_ANCESTRAL_CATEGORY) |>
  group_by(PUBMED_ID,
           STUDY_ACCESSION,
           NUMBER_OF_INDIVIDUALS,
           BROAD_ANCESTRAL_CATEGORY_ORIG) |>
  summarise(n_sample_desc = sum(n))

  summed_sample_desc |>
  filter(n_sample_desc != NUMBER_OF_INDIVIDUALS) |>
  nrow()

  # GCST002466 - difference is that NR is included in European

  summed_sample_desc |>
    filter(n_sample_desc != NUMBER_OF_INDIVIDUALS)  |>
    pull(PUBMED_ID) |>
    unique() |> length()

  summed_sample_desc |>
    filter(STUDY_ACCESSION == "GCST000027")

  # GCST009169 mistake

  #  gwas_ancest_info |> filter(STUDY_ACCESSION =="GCST000027")
  #  NUMBER_OF_INDIVIDUALS doesn't seem to match INITIAL_SAMPLE_DESCRIPTION
  # n_sample_desc of 6674 seems to match text: https://www.nature.com/articles/ng2043#Sec2


  # for pubmed id; 37821706, GCST90296608
  # n_sample_desc != NUMBER_OF_INDIVIDUALS
  # because some individuals didn't have phenotype information
  # NUMBER_OF_INDIVIDUALS is just the total number of phenotyped individuals
  summed_sample_desc |>
    filter(PUBMED_ID == "37821706") |>
    filter(n_sample_desc != NUMBER_OF_INDIVIDUALS)






  group_by(STUDY_ACCESSION,
           BROAD_ANCESTRAL_CATEGORY,
           NUMBER_OF_INDIVIDUALS,
           INITIAL_SAMPLE_DESCRIPTION) |>
  summarise(sample_ancestry = paste0(unique(sample_ancestry),
                                     collapse = ", "),
            sample_broad_ancestry = paste0(unique(sample_broad_ancestry),
                                           collapse = ", ")
  ) |>
            # BROAD_ANCESTRAL_CATEGORY = paste0(unique(BROAD_ANCESTRAL_CATEGORY),
            #                                   collapse = ", "))|>
  filter(sample_broad_ancestry != BROAD_ANCESTRAL_CATEGORY) |>
  select(sample_broad_ancestry, BROAD_ANCESTRAL_CATEGORY) |>
  View()

# cases where INITIAL_SAMPLE_DESCRIPTION, includes all BROAD_ANCESTRAL_CATEGORY
separate_gwas_n_samples |> filter(STUDY_ACCESSION == "GCST000604")
gwas_ancest_subset |> filter(STUDY_ACCESSION == "GCST000604")

separate_gwas_n_samples |> filter(STUDY_ACCESSION == "GCST000940")


gwas_ancest_subset |> filter(STUDY_ACCESSION == "GCST000940")


separate_gwas_n_samples |>
  filter(grepl("familie[s]|trio[s]",
               INITIAL_SAMPLE_DESCRIPTION))

separate_gwas_n_samples |>
  group_by(STUDY_ACCESSION,
           BROAD_ANCESTRAL_CATEGORY) |>
  summarise(sum_sub_n = sum(n),
            total_n = unique(NUMBER_OF_INDIVIDUALS))

separate_gwas_n_samples |>
  select(STUDY_ACCESSION,
         NUMBER_OF_INDIVIDUALS,
         BROAD_ANCESTRAL_CATEGORY) |>
  distinct() |>
  nrow()

separate_gwas_n_samples |>
  select(STUDY_ACCESSION,
         BROAD_ANCESTRAL_CATEGORY) |>
  distinct() |>
  nrow()


separate_gwas_n_samples |>
  select(STUDY_ACCESSION, NUMBER_OF_INDIVIDUALS, BROAD_ANCESTRAL_CATEGORY) |>
  distinct() |>
  pull(STUDY_ACCESSION) |>
  unique() |>
  length()


gwas_ancest_info |>
  filter(PUBMED_ID %in% training_sample)  |>
  filter(grepl("recipient and other controls", INITIAL_SAMPLE_DESCRIPTION)) |>
  select(INITIAL_SAMPLE_DESCRIPTION)
