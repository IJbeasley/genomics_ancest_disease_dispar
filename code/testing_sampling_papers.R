

# WCSS  k= 16  inertia=11488.21
# WCSS  k= 17  inertia=11397.82
# WCSS  k= 18  inertia=11453.53
#
# Gap  k= 14  gap=1.4986 ± 0.0008
# Gap  k= 15  gap=1.5042 ± 0.0009
# Gap  k= 16  gap=1.5046 ± 0.0014
# Gap  k= 17  gap=1.5113 ± 0.0006
# Gap  k= 18  gap=1.5043 ± 0.0009


# https://pmc.ncbi.nlm.nih.gov/articles/PMC4934373/
# https://www.cambridge.org/core/journals/natural-language-processing/article/statistical-dataset-evaluation-a-case-study-on-named-entity-recognition/BF7FA3EF95004830F233CF5D743D98B2
# https://pmc.ncbi.nlm.nih.gov/articles/PMC11491619/
{
pattern <- paste(
  "meta-analy[sz]is",           # meta-analysis / meta-analyses / meta-analyze
  "meta-analy[sz]ed",
  "meta-analy[sz]ing",
  "metaanaly[sz]is",            # no hyphen
  "meta analy[sz]is",           # space
  # "pooled analysis",
  # "pooled analyses",
  # "combined analysis",
  # "combined analyses",
  # "joint analysis",
  # "mega-analysis",              # individual-participant variant
 "METAL",
  sep = "|"
)


possible_meta_abstracts <-
purrr::keep(abstracts,
             ~ any(grepl(pattern, .x, ignore.case = TRUE))) |>
  names()

# purrr::map(abstracts,
#             ~grep(pattern, .x, ignore.case = TRUE, value = TRUE)) |>
#   purrr::list_c()

possible_meta_methods <-
  purrr::keep(methods_texts,
              ~ any(grepl(pattern, .x, ignore.case = TRUE))) |>
  names()

possible_meta <- union(possible_meta_abstracts, possible_meta_methods)

methods_cohorts |>
  select(pubmed_id, COHORT) |>
  distinct() |>
  group_by(pubmed_id) |>
  summarise(n_cohorts = length(unique(COHORT))) |>
  mutate(is_meta = pubmed_id %in% possible_meta) |>
  group_by(is_meta) |>
  summarise(mean_n_cohorts = mean(n_cohorts),
            median_n_cohorts = median(n_cohorts),
            max_n_cohorts = max(n_cohorts),
            first_quartile_n_cohorts = quantile(n_cohorts, 0.25),
            n_one_cohort = sum(n_cohorts == 1),
            #min_n_cohorts = min(n_cohorts),
            n = n())
}




gwas_study_info |>
  filter(PUBMED_ID %in% methods_cohorts$pubmed_id) |>
  select(cause, PUBMED_ID) |>
  distinct() |>
  mutate(is_meta = PUBMED_ID %in% possible_meta) |>
  group_by(cause, is_meta) |>
  summarise(n = n())

gwas_study_info |>
  filter(PUBMED_ID %in% methods_cohorts$pubmed_id) |>
  select(cause, PUBMED_ID) |>
  distinct() |>
  filter(PUBMED_ID %in% possible_meta) |>
  group_by(cause)


selected_study_accessions <-
gwas_study_info |>
  pull(STUDY_ACCESSION) |>
  unique()

gwas_ancest_info |>
  filter(STUDY_ACCESSION %in% selected_study_accessions) |>
  group_by(PUBMED_ID, STUDY_ACCESSION) |>
  summarise(n_sample = sum(NUMBER_OF_INDIVIDUALS)) |>
  group_by(PUBMED_ID) |>
  summarise(n = n(),
            n_sample = str_flatten(unique(n_sample), collapse = ", ")
  ) |>
  filter(grepl(",", n_sample))


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


gwas_ancest_info = gwas_ancest_info |>
  dplyr::filter(!is.na(NUMBER_OF_INDIVIDUALS)) |>
  dplyr::filter(NUMBER_OF_INDIVIDUALS != 0)

n_sample <-
gwas_ancest_info |>
  filter(STUDY_ACCESSION %in% selected_study_accessions) |>
  group_by(PUBMED_ID, STUDY_ACCESSION) |>
  summarise(n_sample = sum(NUMBER_OF_INDIVIDUALS)) |>
  group_by(PUBMED_ID) |>
  summarise(n = n(),
            #n_samples = str_flatten(unique(n_sample), collapse = ", "),
            n_sample_order = max(floor(log10(abs(n_sample))))
  ) |>
  mutate(pubmed_id = as.character(PUBMED_ID)) |>
  select(-PUBMED_ID)

n_studies <-
methods_cohorts |>
  select(pubmed_id, STUDY_ACCESSION) |>
  distinct() |>
  group_by(pubmed_id) |>
  summarise(n_studies = n())


n_sample <-
gwas_ancest_info |>
  group_by(PUBMED_ID) |>
  summarise(n_sample = sum(NUMBER_OF_INDIVIDUALS)) |>
  mutate(pubmed_id = as.character(PUBMED_ID)) |>
  select(-PUBMED_ID)

test <-
left_join(
methods_cohorts,
n_sample,
by = "pubmed_id") |>
mutate(COHORT = ifelse(COHORT == "", NA, COHORT)) |>
  select(pubmed_id, YEAR, COHORT, n) |>
  distinct() |>
group_by(pubmed_id) |>
  summarise(COHORT = str_flatten(unique(COHORT), collapse = ", ", na.rm = TRUE),
            YEAR = str_flatten(unique(YEAR), collapse = ", "),
            n_sample = str_flatten(unique(n), collapse = ", ")) |>
  mutate(n_cohorts = str_count(COHORT, ","))

cor(test$n_cohorts, as.numeric(test$YEAR),
    method = "spearman")

cor(test$n_cohorts, as.numeric(test$n_sample),
    method = "spearman")

methods_cohorts <-
filtered_methods_cohort_sentences_df |>
  tidyr::separate_rows(COHORT, sep = "\\|")  |>
  select(pubmed_id, COHORT) |>
  distinct()


cohort_group <-
matched |>
  select(cohort, full_name, synonyms, group_id) |>
  tidyr::pivot_longer(cols = c(cohort, full_name, synonyms),
                      names_to = "field",
                      values_to = "COHORT") |>
  filter(!is.na(COHORT)) |>
  select(-field) |>
  distinct()

methods_cohorts |>
  group_by(YEAR, COHORT) |>
  filter(COHORT != "") |>
  distinct() |>
  group_by(YEAR) |>
  summarise(cohorts = str_flatten(unique(COHORT), collapse = ", ") )

cohort_group <-
  cohort_group |>
  group_by(group_id) |>
  # group name is the shortest cohort name in the group (after trimming whitespace)
  mutate(group_name = COHORT[which.min(nchar(str_trim(COHORT)))][1])

methods_cohorts <-
left_join(methods_cohorts,
          cohort_group |> ungroup() |> select(COHORT, group_name) |> distinct(),
          by = "COHORT") |>
  select(-COHORT) |>
  rename(COHORT = group_name) |>
  mutate(COHORT = ifelse(is.na(COHORT), "", COHORT))


lancet_cause_mapping <- readxl::read_xlsx(here::here("data/icd/lancet_conditions_icd10.xlsx"),
                                          sheet = 1) |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_study_info <- data.table::fread(here::here("output/icd_map/gwas_study_gbd_causes.csv"))

gwas_study_info = gwas_study_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

# filter out infectious diseases
gwas_study_info <- gwas_study_info |>
  dplyr::filter(!cause %in% c("HIV/AIDS",
                              "Tuberculosis",
                              "Malaria",
                              "Lower respiratory infections",
                              "Diarrhoeal diseases",
                              "Neonatal disorders",
                              "Tetanus",
                              "Diphtheria",
                              "Pertussis" ,
                              "Measles",
                              "Maternal disorders"))

gwas_study_info <-
  gwas_study_info |>
  filter(cause != "")


gwas_study_info <-
gwas_study_info |>
  left_join(lancet_cause_mapping |> select(cause = mapped_gbd_term,
                                           lancet_condition),
            by = "cause",
            relationship = "many-to-many")

methods_cohorts <-
  left_join(methods_cohorts,
            gwas_study_info |>
              rename(pubmed_id = PUBMED_ID) |>
              mutate(pubmed_id = as.character(pubmed_id)),
            by = "pubmed_id")

# unseen entity proportion
full_set <- methods_cohorts |>
  select(pubmed_id) |>
  distinct() |>
  pull(pubmed_id) |>
  unique()

full_set_cohorts <- methods_cohorts |>
  filter(COHORT != "") |>
  tidyr::separate_rows(COHORT, sep = "\\|")  |>
  select(COHORT) |>
  pull(COHORT) |>
  unique()

library(dplyr)
library(tidyr)
library(purrr)

# --- 1. Fix a target test-set size ----------------------------------------
target_n <- 70   # pick whatever makes sense for your eval budget

# --- 2. Helper: compute unseen-cohort proportion given pubmed_ids ---------
compute_unseen <- function(test_ids, data, full_cohorts) {
  test_cohorts <- data |>
    filter(pubmed_id %in% test_ids, COHORT != "") |>
    separate_rows(COHORT, sep = "\\|") |>
    pull(COHORT) |>
    unique()
  length(setdiff(full_cohorts, test_cohorts)) / length(full_cohorts)
}

# --- 3. Define samplers that ALL return exactly `target_n` pubmed_ids ------
# Strategies that naturally give too many papers: downsample to target_n.
# Strategies that give too few: draw extra at random from unused papers.

trim_to_n <- function(ids, data, n) {
  ids <- unique(ids)
  if (length(ids) >= n) {
    sample(ids, n)
  } else {
    extras <- setdiff(unique(data$pubmed_id), ids)
    c(ids, sample(extras, n - length(ids)))
  }
}

strategies <- list(
  random = \(d) d |> distinct(pubmed_id) |>
    slice_sample(n = target_n) |> pull(pubmed_id),

  stratified_lancet = \(d) {
    ids <- d |> distinct(lancet_condition, pubmed_id) |>
      slice_sample(prop = 0.25, by = lancet_condition) |> pull(pubmed_id)
    trim_to_n(ids, d, target_n)
  },

  stratified_year = \(d) {
    ids <- d |> distinct(YEAR, pubmed_id) |>
      slice_sample(n = 4, by = YEAR) |> pull(pubmed_id)
    trim_to_n(ids, d, target_n)
  },

  stratified_year_lancet = \(d) {
    ids <- d |> distinct(YEAR, lancet_condition, pubmed_id) |>
      slice_sample(n = 1, by = c(lancet_condition, YEAR)) |> pull(pubmed_id)
    trim_to_n(ids, d, target_n)
  },

  stratified_cause = \(d) {
    ids <- d |> distinct(cause, pubmed_id) |>
      slice_sample(n = 4, by = cause) |> pull(pubmed_id)
    trim_to_n(ids, d, target_n)
  }
)

# --- 4. Run all strategies across the SAME set of seeds -------------------
n_iter <- 100
seeds <- sample.int(1e6, n_iter)

results <- map_dfr(names(strategies), \(name) {
  map_dfr(seeds, \(s) {
    set.seed(s)
    ids <- unique(strategies[[name]](methods_cohorts))
    tibble(
      strategy = name,
      seed     = s,
      n_papers = length(ids),
      unseen   = compute_unseen(ids, methods_cohorts, full_set_cohorts)
    )
  })
})

# --- 5. Compare ------------------------------------------------------------
results |>
  group_by(strategy) |>
  summarise(mean_unseen   = mean(unseen),
            median_unseen = median(unseen),
            sd_unseen     = sd(unseen),
            mean_n        = mean(n_papers)) |>
  arrange(mean_unseen)

library(dplyr)
library(tidyr)
library(purrr)

# helper: given a sampling function that returns a vector of pubmed_ids,
# compute the unseen-cohort proportion over n_iter replicates
unseen_prop <- function(sampler, data, full_cohorts, n_iter = 100) {
  replicate(n_iter, {
    test_ids <- sampler(data)
    test_cohorts <- data |>
      filter(pubmed_id %in% test_ids, COHORT != "") |>
      separate_rows(COHORT, sep = "\\|") |>
      pull(COHORT) |>
      unique()
    length(setdiff(full_cohorts, test_cohorts)) / length(full_cohorts)
  })
}

# define each sampling strategy as a function
strategies <- list(
  random_30pct = \(d) d |> distinct(pubmed_id) |>
    slice_sample(prop = 0.3) |> pull(pubmed_id),

  stratified_lancet = \(d) d |> distinct(lancet_condition, pubmed_id) |>
    slice_sample(prop = 0.25, by = lancet_condition) |> pull(pubmed_id) |> unique(),

  stratified_year = \(d) d |> distinct(YEAR, pubmed_id) |>
    slice_sample(n = 4, by = YEAR) |> pull(pubmed_id) |> unique(),

  stratified_year_lancet = \(d) d |> distinct(YEAR, lancet_condition, pubmed_id) |>
    slice_sample(n = 1, by = c(lancet_condition, YEAR)) |> pull(pubmed_id) |> unique(),

  stratified_cause = \(d) d |> distinct(cause, pubmed_id) |>
    slice_sample(n = 4, by = cause) |> pull(pubmed_id) |> unique()
)

set.seed(42)
results <- map(strategies, \(f) unseen_prop(f, methods_cohorts, full_set_cohorts, 100))

# summary table
summary_tbl <- imap_dfr(results, \(v, name) tibble(
  strategy = name,
  mean     = mean(v),
  median   = median(v),
  sd       = sd(v),
  min      = min(v),
  max      = max(v),
  mean_n   = mean(map_dbl(1:20, \(i) length(strategies[[name]](methods_cohorts))))
)) |> arrange(mean)

summary_tbl

library(ggplot2)
imap_dfr(results, \(v, name) tibble(strategy = name, unseen = v)) |>
  ggplot(aes(strategy, unseen)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "unseen cohort proportion (100 reps)", x = NULL)


unseen_prop <- c()

for(i in 1:100) {

test_set <- methods_cohorts |>
  select(pubmed_id) |>
  distinct() |>
  dplyr::slice_sample(prop = 0.3) |>
  pull(pubmed_id) |>
  unique()

test_set_cohorts <- methods_cohorts |>
  filter(pubmed_id %in% test_set,
         COHORT != "") |>
  tidyr::separate_rows(COHORT, sep = "\\|")  |>
  select(COHORT) |>
  pull(COHORT) |>
  unique()

unseen_prop[i] <- length(setdiff(full_set_cohorts, test_set_cohorts)) / length(full_set_cohorts)

}

unseen_prop |> summary()

for(i in 1:100) {

  test_set <- methods_cohorts |>
    select(lancet_condition, pubmed_id) |>
    distinct() |>
    dplyr::slice_sample(prop = 0.25, by = lancet_condition) |>
    pull(pubmed_id) |>
    unique()

  test_set_cohorts <- methods_cohorts |>
    filter(pubmed_id %in% test_set,
           COHORT != "") |>
    tidyr::separate_rows(COHORT, sep = "\\|")  |>
    select(COHORT) |>
    pull(COHORT) |>
    unique()

  unseen_prop[i] <- length(setdiff(full_set_cohorts, test_set_cohorts)) / length(full_set_cohorts)

}

length(test_set)

unseen_prop |> summary()

for(i in 1:100) {

  test_set <- methods_cohorts |>
    select(YEAR, pubmed_id) |>
    distinct() |>
    dplyr::slice_sample(n = 4, by = YEAR) |>
    pull(pubmed_id) |>
    unique()

  test_set_cohorts <- methods_cohorts |>
    filter(pubmed_id %in% test_set,
           COHORT != "") |>
    tidyr::separate_rows(COHORT, sep = "\\|")  |>
    select(COHORT) |>
    pull(COHORT) |>
    unique()

  unseen_prop[i] <- length(setdiff(full_set_cohorts, test_set_cohorts)) / length(full_set_cohorts)

}

length(test_set)

unseen_prop |> summary()

for(i in 1:100) {

  test_set <- methods_cohorts |>
    select(YEAR, lancet_condition, pubmed_id) |>
    distinct() |>
    dplyr::slice_sample(n = 1, by = c(lancet_condition, YEAR)) |>
    pull(pubmed_id) |>
    unique()

  test_set_cohorts <- methods_cohorts |>
    filter(pubmed_id %in% test_set,
           COHORT != "") |>
    tidyr::separate_rows(COHORT, sep = "\\|")  |>
    select(COHORT) |>
    pull(COHORT) |>
    unique()

  unseen_prop[i] <- length(setdiff(full_set_cohorts, test_set_cohorts)) / length(full_set_cohorts)

}

length(test_set)

unseen_prop |> summary()

for(i in 1:100) {

  test_set <- methods_cohorts |>
    select(cause, pubmed_id) |>
    distinct() |>
    dplyr::slice_sample(n = 4, by = c(cause)) |>
    pull(pubmed_id) |>
    unique()

  test_set_cohorts <- methods_cohorts |>
    filter(pubmed_id %in% test_set,
           COHORT != "") |>
    tidyr::separate_rows(COHORT, sep = "\\|")  |>
    select(COHORT) |>
    pull(COHORT) |>
    unique()

  unseen_prop[i] <- length(setdiff(full_set_cohorts, test_set_cohorts)) / length(full_set_cohorts)

}

length(test_set)

unseen_prop |> summary()

setdiff()

sets <-
gwas_study_info |>
  group_by(lancet_condition) |>
  summarise(PUBMED_ID = list(unique(PUBMED_ID))) |>
  tibble::deframe()


combined_df <-
left_join(methods_cohort_sentences_df,
          gwas_study_info |> rename(pubmed_id = PUBMED_ID) |>
            mutate(pubmed_id = as.character(pubmed_id)),
          by = "pubmed_id") |>
  filter(COHORT != "") |>
  tidyr::separate_rows(COHORT, sep = "\\|")


sets <-
  combined_df |>
  group_by(lancet_condition,YEAR) |>
  summarise(cohorts = list(unique(COHORT))) |>
  tibble::deframe()


m1 = make_comb_mat(sets)

comb_size(m1, degree = 1)  |> summary()

UpSet(m1)


|>
  group_by(YEAR) |>
  summarise(cohorts = list(unique(COHORT)))
gwas_study_info)






left_join(methods_cohort_sentences_df,
          gwas_study_info |> select(-COHORT,
                                    pubmed_id = PUBMED_ID) |>
            mutate(pubmed_id = as.character(pubmed_id)),
          by = "pubmed_id") |>
  filter(COHORT != "") |>
  tidyr::separate_rows(COHORT, sep = "\\|") |>






#get list of unique cohorts per year

left_join(methods_cohort_sentences_df,
          gwas_study_info |> select(-COHORT,
                                    pubmed_id = PUBMED_ID) |> mutate(pubmed_id = as.character(pubmed_id)),
          by = "pubmed_id") |>
  filter(COHORT != "") |>
  #select(COHORT, article_id) |>
  tidyr::separate_rows(COHORT, sep = "\\|") |>
  group_by(YEAR) |>
  summarise(cohorts = c(unique(COHORT)))
            #n_articles = length(unique(pubmed_id))) |>


  pull()
  distinct() |>
  group_by(COHORT) |>
  summarise(n_abstracts = n())

  sets <-
  left_join(methods_cohort_sentences_df,
            gwas_study_info |> select(-COHORT, pubmed_id = PUBMED_ID) |> mutate(pubmed_id = as.character(pubmed_id)),
            by = "pubmed_id") |>
    filter(COHORT != "") |>
    tidyr::separate_rows(COHORT, sep = "\\|") |>
    group_by(YEAR) |>
    summarise(cohorts = list(unique(COHORT))) |>
    tibble::deframe()

  library(ComplexHeatmap)

  m1 = make_comb_mat(sets)

  UpSet(m1)

  long_df <- left_join(methods_cohort_sentences_df,
                       gwas_study_info |> select(-COHORT, pubmed_id = PUBMED_ID) |> mutate(pubmed_id = as.character(pubmed_id)),
                       by = "pubmed_id") |>
    filter(COHORT != "") |>
    tidyr::separate_rows(COHORT, sep = "\\|") |>
    distinct(YEAR, COHORT, pubmed_id)

  unique_cohorts <-
  long_df |>
    group_by(COHORT) |>
    filter(n() == 1)

  long_df |>
    filter(COHORT %in% unique_cohorts$COHORT) |>
    group_by(YEAR) |>
    summarise(cohorts = length(unique(COHORT)),
              n_articles = length(unique(pubmed_id))) |>
    mutate(cohorts_per_article = cohorts / n_articles)

  long_df |>
    group_by(COHORT) |>
    filter(n() == 1) |>        # cohort appears in only one year
    ungroup() |>
    group_by(YEAR) |>
    summarise(cohorts = list(COHORT)) |>
    tibble::deframe()
