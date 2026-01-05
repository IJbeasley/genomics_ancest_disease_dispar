
# Example data
# nih funding
nih_funds <- readxl::read_xlsx(here::here("data/RCDCFundingSummary_01042026.xlsx"),
                               skip = 1,
                               n_max = 328)

nih_funds =
  nih_funds |>
  filter(`2019` != "+") |>
  mutate(`2019` = as.numeric(`2019`))

nih_funds |>
  pull(`2019`) |>
  hist(main = "NIH funding amounts per RCDC category (2019)",
       xlab = "Funding amount (USD)",
       ylab = "# RCDC categories"
       )

nih_funds |>
  pull(`2019`) |>
  log10() |>
  hist(main = "log10(NIH funding amounts per RCDC category (2019))",
       xlab = "Funding amount (USD)",
       ylab = "# RCDC categories"
  )

nih_funds <-
  nih_funds |>
  mutate(log10_2019 = log10(`2019`)) |>
  select(`2019`,
         log10_2019,
         condition = `Research/Disease Areas \n (Dollars in millions and rounded)`)


# racial bias
age_adj_rate_ratio # from test_extract_cdc_meta

age_adj_rate_ratio |>
  hist(main = "Age-adjusted rate black / white ratio per disease (CDC)",
       xlab = "Age-adjusted rate ratio",
       ylab = "# diseases"
  )

conditions <-
black_rate_summary$icd_10_codes_group

racial_bias <- data.frame(age_adj_rate_ratio,
                          condition = conditions)

# exponential distribution?


# from disease_inves_by_ancest
# number of participants
# gbd daly
compare_stats =
compare_stats |>
  select(total_n,
         condition = cause,
         gbd_daly = val)

compare_stats$total_n |>
  hist(main = "Total number of participants per disease (GWAS Catalog)",
       xlab = "Total number of participants",
       ylab = "# diseases"
  )

compare_stats$total_n |> mean()
compare_stats$total_n |> var()

# maybe best modelled with neg binomial?

# gbd_daly
compare_stats$gbd_daly |>
  hist(main = "GBD DALYs per disease",
       xlab = "DALYs",
       ylab = "# diseases"
  )

compare_stats$gbd_daly |>
  log10() |>
  hist(main = "log10(GBD DALYs per disease)",
       xlab = "DALYs",
       ylab = "# diseases"
  )

compare_stats$gbd_daly |> mean(na.rm = T)
compare_stats$gbd_daly |> var(na.rm = T)

# maybe best modelled with neg binomial?


# put into one data.frame

nih_funds =
  nih_funds |>
  mutate(condition = str_replace(pattern = "Diabetes 5",
                                 replace = "Diabetes mellitus",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "HIV/AIDS 8",
                                 replace = "HIV/AIDS",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Lung Cancer",
                                 replace = "Tracheal, bronchus, and lung cancer",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Stroke",
                                 replace = "Ischemic stroke",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Chronic Obstructive Pulmonary Disease",
                                 replace = "Chronic obstructive pulmonary disease",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Chronic Liver Disease and Cirrhosis",
                                 replace = "Cirrhosis and other chronic liver diseases",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Heart Disease - Coronary Heart Disease",
                                 replace = "Ischemic heart disease",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Liver cancer",
                                 replace = "Liver cancer due to hepatitis B",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Liver cancer",
                                 replace = "Liver cancer due to hepatitis C",
                                 condition))


nih_funds |>
  filter(condition == "Neonatal Respiratory Distress" |
         condition == "Preterm, Low Birth Weight and Health of the Newborn"
         )

nih_funds =
  nih_funds |>
  add_row(
    condition = "Neonatal disorders",
    `2019` = nih_funds |>
      filter(condition == "Neonatal Respiratory Distress" |
               condition == "Preterm, Low Birth Weight and Health of the Newborn"
             ) |>
      pull(`2019`) |>
      sum(),
    log10_2019 = log10(nih_funds |>
      filter(condition == "Neonatal Respiratory Distress" |
               condition == "Preterm, Low Birth Weight and Health of the Newborn"
             ) |>
      pull(`2019`) |>
      sum()
             )
    )

nih_funds |>
  filter(condition == "Cervical Cancer" |
           condition == "HPV and/or Cervical Cancer Vaccines"
  )

nih_funds =
  nih_funds |>
  add_row(
    condition = "Cervical cancer",
    `2019` = nih_funds |>
      filter(condition == "Cervical Cancer" |
               condition == "HPV and/or Cervical Cancer Vaccines"
      ) |>
      pull(`2019`) |>
      sum(),
    log10_2019 = log10(nih_funds |>
                         filter(condition == "Cervical Cancer" |
                                  condition == "HPV and/or Cervical Cancer Vaccines"
                         ) |>
                         pull(`2019`) |>
                         sum()
    )
  )

compare_stats_nih =
left_join(compare_stats,
          nih_funds)


racial_bias =
  racial_bias |>
  mutate(condition = str_replace(pattern = "Diabetes mellitus type 2",
                                 replace = "Type 2 diabetes mellitus",
                                 condition)) |>
  mutate(condition = str_replace(pattern = "Diabetes mellitus type 1",
                                 replace = "Type 1 diabetes mellitus",
                                 condition))

compare_stats_all =
  left_join(compare_stats_nih,
            racial_bias,
            by = "condition")

# piecewise SEM
library(piecewiseSEM)
library(MASS)

sem_model =
  psem(
    # National Disease Burden ~ Disease Demographics
    glm(gbd_daly ~ age_adj_rate_ratio,
        data = compare_stats_all,
        family = negative.binomial(theta = 1)),
    # NIH Funding ~ National Disease Burden + Disease Demographics
    lm(log10_2019 ~ gbd_daly + age_adj_rate_ratio,
        data = compare_stats_all),
    # GWAS Participants ~ NIH Funding + National Disease Burden + Disease Demographics
    glm(total_n ~ log10_2019 + gbd_daly, #+ age_adj_rate_ratio,
        data = compare_stats_all,
        family = negative.binomial(theta = 1))
  )

basisSet(sem_model)

dSep(sem_model, .progressBar = FALSE)


AIC(sem_model, AIC.type = "dsep")
