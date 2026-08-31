


data.table::fread(here::here("output/fulltexts/pmid_to_pmcid_mapping.csv"))  -> old_pmids

old_pmids <- old_pmids$PMID |> unique()

# new pmids

gwas_study_info <- data.table::fread(
here::here("output/icd_map/gwas_study_gbd_causes.csv")
)

gwas_study_info = gwas_study_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_study_info = gwas_study_info |>
mutate(gbd_term = cause) |>
  mutate(cause = case_when(
    cause %in% c("Liver cancer due to hepatitis B", "Liver cancer due to hepatitis C") ~ "Liver cancer",
    cause %in% c("Chronic kidney disease due to diabetes mellitus type 1",
                 "Chronic kidney disease due to diabetes mellitus type 2") ~ "Chronic kidney disease",
    TRUE ~ cause
  ))


gwas_study_info <- gwas_study_info |>
  # filter out not mapped to causes
  dplyr::filter(lancet_group != "")  |>
  # filter out infectious diseases
  dplyr::filter(lancet_group != "I-8") |>
  # filter out road injury, suicide
  dplyr::filter(!lancet_condition %in% c("Suicide",
                                         "Road injury"))

# (1) Ischaemic heart disease,
# (2) Ischaemic stroke,
# (3) Intracerebral haemorrhage,
# (4) Subarachnoid haemorrhage,
# (5) Diabetes mellitus,
# (6) Type 1 diabetes mellitus,
# (7) Type 2 diabetes mellitus,
# 8 Chronic kidney disease due to diabetes mellitus type 1,
# 9 Chronic kidney disease due to diabetes mellitus type 2,
# 10 Stomach cancer,
# 11 Liver cancer due to hepatitis B,
# 12 Liver cancer due to hepatitis C,
# 13 Cervical cancer,
# 14 Rheumatic heart disease,
# 15 Cirrhosis and other chronic liver diseases,
# 16 Chronic obstructive pulmonary disease,
# 17 Lip and oral cavity cancer,
# 18 Nasopharynx cancer,
# 19 Other pharynx cancer,
# 20 Larynx cancer,
# 21 Tracheal, bronchus, and lung cancer

gwas_study_info  |>
  select(lancet_condition, cause) |>
  arrange(lancet_condition) |>
  pull(cause) |>
  unique()

# 19 causes after collapsing and filtering


gwas_study_info |> pull(PUBMED_ID) |> unique() -> new_pmids


all(old_pmids %in% new_pmids)

old_pmids[!(old_pmids %in% new_pmids)]

new_ids[!(new_ids %in% old_pmids)]

> new_ids[!(new_ids %in% old_pmids)]
# [1] 17395743 17903292 19304780 19349983 19430479 19430482 19609347 19929986 20383146 20532800 20595679 20668430 20686651 20877124 21082022 21323541 21355061
# [18] 21399633 21441931 21546767 21698141 21750111 21909115 22004137 22197929 22322875 22384028 22479191 22479346 22566498 22737229 23300138 23539754 23555189
# [35] 23760081 24084763 24162738 24351856 24714607 24925725 24940741 25249183 25305756 25493955 25695618 25802187 26028593 26029870 26083657 26390057 26420894
# [52] 26980576 27244555 27333618 27480026 27576016 27618447 27618448 27665939 27802415 28273873 28498854 28656603 28871152 29124443 29228715 29523524 29545352
# [69] 29580174 29779033 29885931 29903748 29973135 30012571 30181573 30220432 30237584 30410027 30487518 30510241 30476138 30955190 31015462 31033190 31055733
# [86] 31095341 31152163 31178898 31263063 31423876 31426789 31545351 31596850 31748705 31754133 31879980 31910446 31959995 32231244 32277301 32491161 32554042
# [103] 32572055 32573827 32709000 32912934 33230300 33293549 34026292 33593824 33667223 33838163 34047475 34067580 34076728 34083597 34127828 34197840 34231218
# [120] 34593835 34616010 34670813 34671089 34934334 35120996 35448080 35588731 35652341 35743677 35760791 35803233 35815403 35870639 36057693 36124557 36167494
# [137] 36217425 36238604 36250097 36271344 36275661 36546557 36551779 36623684 37110199 37120605 37124606 37151119 37273234 37277652 37337107 37547536 37568739
# [154] 37787447 37958966 38104120 38116116 38560502 38233393 38425181 38459180 38626723 38701081 39169618 39343836 39636799 39639588 39927731
#

updated_new_ids[!(updated_new_ids %in% old_pmids)]

# 23555189

> updated_new_ids[!(updated_new_ids %in% old_pmids)]
# [18] 22566498 22737229 23300138 23539754 23555189 23760081 24084763 24162738 24351856 24714607 24925725 24940741 25305756
# [35] 25493955 25695618 25802187 26028593 26029870 26083657 26420894 26980576 27244555 27333618 27576016 27665939 27802415 28498854 28656603 29124443 29228715
# [52]
# [69]   31748705 31754133 31959995 32231244 32277301 32554042 32572055 32573827
# [86] 32912934 33293549 33593824 33667223 33838163 34067580 34076728 34127828 34197840 34231218 34593835 34616010 34670813 34671089 34934334 35120996 35448080
# [103] 35588731 35743677 35760791 35803233 35815403 35870639 36057693 36124557 36167494 36238604 36250097 36271344 36275661 36546557 36551779 36623684 37110199
# [120] 37120605 37124606 37151119 37273234 37277652 37337107 37547536 37568739 37787447 37958966 38116116 38560502 38233393 38425181 38459180 38626723 38701081
# [137] 39169618 39343836 39636799 39639588 39927731

# 31095341 tbd
# 31596850

# 20668430

# 21399633

# 20595679 - IgA Nephropathy
# 21399633 - IgA Nephropathy
# 22197929
# 39343836
# 33593824
#  31426789

# 21546767 - Type 2 diabetes missed


# 21355061
# proteinuria
# alu
# hematuria
# Proteinuria (PheCode 269)
# Hematuria (PheCode 593)
# ICD10 N02: Recurrent and persistent hematuria

# 19349983
# chronic vs
# pubmed ids: 19349983
# 21750111
#  22004137

# 22737229


added <- new_ids[!(new_ids %in% old_pmids)]

gwas_study_info <- data.table::fread(
  here::here("output/gwas_cat/gwas_study_info_trait_cat.csv")
)

gwas_study_info = gwas_study_info |>
  dplyr::rename_with(~ gsub(" ", "_", .x))


gwas_study_info |>
  filter(PUBMED_ID == "18075462")

gwas_study_info |>
  filter(PUBMED_ID == "20060832")


disease_mapping <- data.table::fread(here::here("output/icd_map/gwas_disease_to_icd10_mapping.csv"))
disease_mapping |> filter(PUBMED_ID == "18075462")

