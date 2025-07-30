
library(ggplot2)

# number of studies reported twice
# n_reported_studies = data |>
#   dplyr::group_by(PUBMEDID, COUNTRY_OF_RECRUITMENT, STAGE, BROAD_ANCESTRAL_CATEGORY) |>
#   dplyr::summarise(n = dplyr::n())

n_reported_studies  = data |>
  dplyr::select(PUBMEDID,
                COUNTRY_OF_RECRUITMENT,
                STAGE,
                NUMBER_OF_INDIVDUALS,
                BROAD_ANCESTRAL_CATEGORY) |>
  dplyr::distinct() |>
  dplyr::group_by(PUBMEDID,
                  STAGE,
                  COUNTRY_OF_RECRUITMENT,
                  NUMBER_OF_INDIVDUALS, #assume number of
                  BROAD_ANCESTRAL_CATEGORY
                  ) |>
  dplyr::summarise(n_studies = dplyr::n())

n_reported_studies |>
  dplyr::slice_sample() |>
  dplyr::ungroup() |>
  dplyr::group_by(COUNTRY_OF_RECRUITMENT) |>
  dplyr::summarise(n_studies_country = sum(n_studies)) |>
  dplyr::filter(N_country_total > 1) |>
  dplyr::arrange(desc(N_country_total))

n_reported_studies |>
  dplyr::filter(n >= 2) |>
  dplyr::ungroup() |>
  dplyr::slice_sample(n = 2)

n_reported_studies |> head()


data |> dplyr::filter(PUBMEDID == 28394258,
                      BROAD_ANCESTRAL_CATEGORY == "European",
                      STAGE == "initial")

n_reported_studies |>
  dplyr::filter(n > 100) |>
  dplyr::ungroup() |>
  dplyr::slice_sample(n = 2)

sum(n_reported_studies$n > 1)

sum(n_reported_studies$n > 100)
sum(n_reported_studies$n > 50)
sum(n_reported_studies$n > 10)

max(n_reported_studies$n)

summary(n_reported_studies$n)

n_reported_studies |>
  dplyr::filter(n < 100) |>
  ggplot(aes(x = n)) +
  geom_histogram()

data2 = data |>
  dplyr::group_by(PUBMEDID, BROAD_ANCESTRAL_CATEGORY, COUNTRY_OF_RECRUITMENT) |>
  dplyr::slice_max(NUMBER_OF_INDIVDUALS) |>
  dplyr::ungroup() |>
  dplyr::group_by(PUBMEDID,
                  COUNTRY_OF_RECRUITMENT) |>
  dplyr::summarise(N_TOTAL = sum(NUMBER_OF_INDIVDUALS))


data2 |> dplyr::ungroup() |>
         dplyr::group_by(COUNTRY_OF_RECRUITMENT) |>
         dplyr::summarise(N_country = sum(N_TOTAL))
