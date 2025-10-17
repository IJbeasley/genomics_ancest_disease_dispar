
# fixing ancestry multiples or missing
{
gwas_ancest_info <-  fread(here::here("data/gwas_catalog/gwas-catalog-v1.0.3.1-ancestries-r2025-07-21.tsv"),
                           sep = "\t",
                           quote = "")

gwas_ancest_info = gwas_ancest_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_ancest_info = gwas_ancest_info |>
  dplyr::arrange(DATE)

# get only disease studies
gwas_study_info <- fread(here::here("output/gwas_cat/gwas_study_info_trait_group_l2.csv"))

gwas_study_info = gwas_study_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_study_info =
  gwas_study_info |>
  select(STUDY_ACCESSION,
         PUBMED_ID,
         `DISEASE/TRAIT`,
         DISEASE_STUDY,
         PAPER_TITLE = STUDY,
         collected_all_disease_terms) |>
  distinct()


# get cohort information:
gwas_study_info_cohort =
  data.table::fread(here::here("output/gwas_cohorts/gwas_cohort_name_corrected.csv"))

gwas_study_info_cohort =
  gwas_study_info_cohort |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_study_info_cohort =
  gwas_study_info_cohort |>
  select(STUDY_ACCESSION,
         COHORT) |>
  distinct()

gwas_study_info =
  left_join(gwas_study_info,
            gwas_study_info_cohort,
            by = "STUDY_ACCESSION"
            )


gwas_ancest_info =
  left_join(gwas_ancest_info,
            gwas_study_info,
            by = c("STUDY_ACCESSION",
                   "PUBMED_ID")
  )

gwas_ancest_info =
  gwas_ancest_info |>
  filter(DISEASE_STUDY == T)
}


########### Filling in cohort with title #################

gwas_ancest_info |>
  filter(COHORT == "") |>
  filter(grepl("UK Biobank", PAPER_TITLE, ignore.case = T)) |>
  pull(PUBMED_ID) |> unique() |> length()

gwas_ancest_info =
gwas_ancest_info |>
  mutate(COHORT = ifelse(COHORT == "" &
                         grepl("UK Biobank", PAPER_TITLE, ignore.case = T), #&
                         #STAGE == "initial",
                       "UKBB",
                       COHORT
                       )
         )

# Million Veteran Program
gwas_ancest_info |>
  filter(COHORT == "") |>
  filter(grepl("Million Veteran Program", PAPER_TITLE, ignore.case = T)) |>
  pull(PAPER_TITLE) |> unique()

gwas_ancest_info =
  gwas_ancest_info |>
  mutate(COHORT = ifelse(COHORT == "" &
                         grepl("Million Veteran Program", PAPER_TITLE, ignore.case = T), #&
                         STAGE == "initial",
                         "MVP",
                         COHORT
  )
  )

# PUBMED_ID = 29892013
# UK Biobank
gwas_ancest_info =
  gwas_ancest_info |>
  mutate(COHORT = ifelse(COHORT == "" &
                         PUBMED_ID == "29892013",
                       "UKBB",
                       COHORT
  )
  )




# pubmed id: 34522458
# replace NR in BROAD_ANCESTRAL_CATEGORY with East Asian
gwas_ancest_info =
  gwas_ancest_info |>
  mutate(BROAD_ANCESTRAL_CATEGORY = ifelse(PUBMED_ID %in% "34522458",
                         str_replace_all(BROAD_ANCESTRAL_CATEGORY, "NR", "East Asian"),
                       BROAD_ANCESTRAL_CATEGORY
  )
  )


cohorts <-
gwas_ancest_info |>
  filter(COHORT != "") |>
  select(PUBMED_ID, COHORT) |>
  distinct() |>
  group_by(PUBMED_ID) |>
  summarise(COHORT = paste(unique(unlist(
                                  strsplit(COHORT,
                                           split = "\\|")
                                         ),
                                  collapse = "|")
                           )
            ) |>
  pull(COHORT)

all_cohorts = unlist(strsplit(cohorts, split = "\\|"))
all_cohorts = trimws(all_cohorts)

data.frame(cohort = all_cohorts) |>
  group_by(cohort) |>
  summarise(n_studies = n()) |>
  arrange(desc(n_studies)) |>
  head(20)





# PUBMED_ID = 29532581
# if STAGE = "initial"
# FinnTwin
# if STAGE = "replication"
# FinnTwin12


# GIANT

gwas_ancest_info =
  gwas_ancest_info |>
  mutate(COHORT = ifelse(COHORT == "" &
                         grepl("23andMe", PAPER_TITLE, ignore.case = T), #&
                         #STAGE == "initial",
                       "23andMe",
                       COHORT
  )

# ~3,500
# number of papers with at least one missing cohort
gwas_ancest_info |>
  group_by(PUBMED_ID) |>
  filter(COHORT == "") |>
  select(PUBMED_ID) |>
  distinct() |>
  summarise(n = n()) |>
  filter(n > 0) |>
  nrow()

# missing number of individuals
{
missing_sample_size =
gwas_ancest_info |>
  filter(NUMBER_OF_INDIVIDUALS == "" | is.na(NUMBER_OF_INDIVIDUALS))

extract_initial_sample_size =
missing_sample_size |>
  filter(STAGE == "initial") |>
  pull(INITIAL_SAMPLE_DESCRIPTION) |>
  str_remove_all(",") |>
  str_remove_all("\\s*\\([^)]*\\)") |>
  str_extract_all("\\d+")

extract_initial_sample_size = lapply(extract_initial_sample_size,
                                     function(x) sum(as.numeric(x))
                                     )

extract_initial_sample_study_accession =
missing_sample_size |>
  filter(STAGE == "initial") |>
  pull(STUDY_ACCESSION)


extract_replication_sample_size =
  missing_sample_size |>
  filter(STAGE == "replication") |>
  pull(REPLICATION_SAMPLE_DESCRIPTION) |>
  str_remove_all(",") |>
  str_remove_all("\\s*\\([^)]*\\)") |>
  str_extract_all("\\d+")

extract_replication_sample_size = lapply(extract_replication_sample_size,
                                     function(x) sum(as.numeric(x))
)

extract_replication_sample_study_accession =
  missing_sample_size |>
  filter(STAGE == "replication") |>
  pull(STUDY_ACCESSION)

to_add_sample_size =
data.frame(STUDY_ACCESSION = c(unlist(extract_initial_sample_study_accession),
                                 unlist(extract_replication_sample_study_accession)),
           NUMBER_OF_INDIVIDUALS = c(unlist(extract_initial_sample_size),
                                     unlist(extract_replication_sample_size))
           )

to_add_sample_size =
  to_add_sample_size |>
  filter(NUMBER_OF_INDIVIDUALS > 0)

gwas_ancest_info = gwas_ancest_info |>
  rows_update(to_add_sample_size,
              by = "STUDY_ACCESSION")

# remaining missing sample size:
gwas_ancest_info |>
filter(NUMBER_OF_INDIVIDUALS == "" | is.na(NUMBER_OF_INDIVIDUALS))
}

# for study accession = GCST001070
# paper says replication is DIAGRAM +
# Recently, the DIAGRAM consortium expanded
# their meta-analysis to include eight GWA studies in individuals of European ancestry (DIAGRAM+) [1].
# which look at supplement of https://pmc-ncbi-nlm-nih-gov.ucsf.idm.oclc.org/articles/instance/3080658/bin/NIHMS34421-supplement-01.pdf
# is DGDG, deCODE, DGI, Rotterdam, EUROSPAN, FUSION, KORAgen and WTCCC.
# 8,130 cases and 38,987 controls

to_add_manual_sample_size = data.frame(
  STUDY_ACCESSION = c("GCST001070"),
  STAGE = "replication",
  NUMBER_OF_INDIVIDUALS = c(8130 + 38987),
  COHORT = "DGDG|deCODE|DGI|Rotterdam|EUROSPAN|FUSION|KORAgen|WTCCC"

)

# for 34743297 / study accession GCST90095190
# sample is

# cohort = 	PUBMED_ID = GCST002245 | 27225129


source(here::here("code/custom_plotting.R"))

######## Adding cohort #######

cohort = c("GERA|UKBB", "23andMe")
STAGE = c("initial", "replication")
STUDY_ACCESSION = c("GCST012013", "GCST012013")


#
cohort = c("OPPERA")
STUDY_ACCESSION = c("GCST007619")


gwas_ancest_info |>
  select(PUBMED_ID, `DISEASE/TRAIT`, COHORT) |>
  distinct() |>
  group_by(PUBMED_ID, `DISEASE/TRAIT`) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  nrow()


gwas_ancest_info |>
  select(PUBMED_ID, `DISEASE/TRAIT`, COHORT) |>
  distinct() |>
  group_by(PUBMED_ID, `DISEASE/TRAIT`) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  group_by(PUBMED_ID) |>
  summarise(n = n()) |>
  arrange(desc(n))

# PUBMED_ID = 34594039
# all good

# PUBMED_ID = 34734193


# seems likely that within a pubmed id & DISEASE/TRAIT combination.
# sample re-use is corrected for

# What about for within a pubmed id & collected_all_disease_terms
gwas_ancest_info =
  gwas_ancest_info |>
  tidyr::separate_longer_delim(cols = "collected_all_disease_terms",
                               delim = ", "
                               )

# ? Maybe increased ... by like 1 study
gwas_ancest_info |>
  select(PUBMED_ID, collected_all_disease_terms, COHORT) |>
  distinct() |>
  group_by(PUBMED_ID, collected_all_disease_terms) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  nrow()


missing_cohort_per_trait <-
gwas_ancest_info |>
  group_by(collected_all_disease_terms) |>
  filter(COHORT == "") |>
  select(collected_all_disease_terms, PUBMED_ID, COHORT) |>
  distinct() |>
  summarise(n_missing = n())

n_cohort_per_trait <-
  gwas_ancest_info |>
  select(collected_all_disease_terms, PUBMED_ID) |>
  distinct() |>
  group_by(collected_all_disease_terms) |>
  summarise(n_total = n())

missing_cohort_percent =
inner_join(missing_cohort_per_trait,
           n_cohort_per_trait,
           by = "collected_all_disease_terms"
           ) |>
  mutate(percent_missing = n_missing / n_total * 100) |>
  arrange(desc(percent_missing))


# UK


missing_cohort_percent |>
  filter(percent_missing < 100)

# GCST003184
# https://pmc-ncbi-nlm-nih-gov.ucsf.idm.oclc.org/articles/instance/4753676/bin/NIHMS65321-supplement-Supplementary_Information.pdf


gwas_ancest_info |>
  filter(COHORT == "")  |>
  pull(PUBMED_ID) |> unique() -> test
gwas_ancest_info |> filter(PUBMED_ID %in% test) |> filter(COHORT != "")
# GCST90103970
# should undoubtedly be UKBB

gwas_ancest_info =
  gwas_ancest_info |>
  mutate(grouped_ancestry = group_ancestry_fn(BROAD_ANCESTRAL_CATEGORY))

gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  nrow()

gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  pull(PUBMED_ID) |>
  unique() |>
  length()

gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  arrange(desc(NUMBER_OF_INDIVIDUALS))



gwas_ancest_info |>
  filter(PUBMED_ID == "36224396") |>
  select(BROAD_ANCESTRAL_CATEGORY,
         NUMBER_OF_INDIVIDUALS,
         INITIAL_SAMPLE_DESCRIPTION,
         REPLICATION_SAMPLE_DESCRIPTION
         )

# 2200007 European, Hispanic or Latin American, East Asian, African unspecified, South Asian

# 58709 Hispanic or Latin American / 455180 Hispanic or Latin American
# 363856 East Asian / 472730 East Asian
# 60939 South Asian / 77890 South Asian
# 293593 African unspecified


# *The number of individuals in the trans-ancestry meta-analysis (n=5,314,291)
#is smaller than the sum of ancestry-group-specific meta-analyses (n=5,380,080) because of variation in
# per-SNP sample sizes for SNPs included in the final analysis.

# hence, let's estimate by sample ancestry proportions
#  https://pmc-ncbi-nlm-nih-gov.ucsf.idm.oclc.org/articles/PMC9605867/#Fig5


# pubmed id: 36477530
# smoking initiation, multiple = GCST90243985
# likely to similar for same trait here: (0.99)
(2669029 + 296395 + 286026 + 119589) / 3382012









gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  nrow()

pubmed_id_with_multiple <-
  gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  pull(PUBMED_ID) |>
  unique()

total_n_per_trait <-
gwas_ancest_info |>
  filter(PUBMED_ID %in% pubmed_id_with_multiple) |>
  filter(grouped_ancestry != "Multiple") |>
  group_by(PUBMED_ID, `DISEASE/TRAIT`) |>
  summarise(
            total_n = sum(as.numeric(NUMBER_OF_INDIVIDUALS), na.rm = TRUE)
            )

multiple_n_per_trait <-
gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  select(PUBMED_ID, NUMBER_OF_INDIVIDUALS, `DISEASE/TRAIT`) |>
  rename(multiple_n = NUMBER_OF_INDIVIDUALS)

multiple =
left_join(multiple_n_per_trait,
          total_n_per_trait,
          by = c("PUBMED_ID", "DISEASE/TRAIT"),
          relationship = "many-to-one"
          ) |>
  mutate(percent = multiple_n / total_n * 100)

# 34 studies & `DISEASE/TRAIT` pairs where just summing up the ancestries = multiple
multiple |>
  filter(percent == 100) |>
  nrow()


gwas_ancest_info |>
  filter(grouped_ancestry == "Multiple") |>
  group_by(BROAD_ANCESTRAL_CATEGORY) |>
  summarise(n = n())

####### can separate out

# for PUBMED_ID == 37337106
# BROAD_ANCESTRAL_CATEGORY == "European, East Asian"
# NUMBER_OF_INDIVIDUALS == 38977

# 7009 are East Asian,
# 31968 are European
gwas_ancest_info |>
  filter(PUBMED_ID == "37337106") |>
  filter(BROAD_ANCESTRAL_CATEGORY == "European, East Asian") |>
  filter(NUMBER_OF_INDIVIDUALS == 38977) |>
  nrow()
###### ? may be able to separate out

# for pubmed id: 33462485
# BROAD_ANCESTRAL_CATEGORY ==  NR, Other admixed ancestry
# number of individuals = 1531
# 203 (Admixed) + 1328 (Multiethinic) = 1531
gwas_ancest_info |>
  filter(PUBMED_ID == "33462485") |>
  filter(BROAD_ANCESTRAL_CATEGORY == "NR, Other admixed ancestry") |>
  filter(NUMBER_OF_INDIVIDUALS == 1531) |>
  nrow()



