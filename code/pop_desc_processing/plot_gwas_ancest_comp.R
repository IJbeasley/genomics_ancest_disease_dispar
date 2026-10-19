# Functions for simplifying broad ancestry groups in
# GWAS Catalog Metadata
# And for estimating + plotting ancestry composition of
# GWAS Catalog samples

ancestry_colors <- c(
  "African" = "#984EA3",
  "European" = "#E41A1C",
  "Asian" = "#377EB8", #east asian
  # "South Asian" = "#4DAF4A",
  "Hispanic/Latin American" = "#FF7F00",
  "Middle Eastern" = "#FFFF33",
  "Oceanic" = "#A65628",
  "Other" = "#F781BF",
  "Multiple" = "#999999",
  "Not reported" = "black"
)


# Define the desired stacking order
ancestry_levels <- c(
  "European",
  # "East Asian",
  # "South Asian",
  "Asian",
  "African",
  "Hispanic/Latin American",
  "Middle Eastern",
  "Oceanic",
  "Other",
  "Multiple",
  "Not reported"
)


group_ancestry_fn = function(study_ancest){

  case_when(

    # European
    study_ancest %in% c('European') ~ 'European',

    # African
    study_ancest %in% c('Sub-Saharan African, African American or Afro-Caribbean',
                        'African unspecified, African American or Afro-Caribbean',
                        'African American or Afro-Caribbean, African unspecified',
                        'Sub-Saharan African, African unspecified',
                        'African unspecified, Sub-Saharan African',
                        'African-American or Afro-Caribbean',
                        'Sub-Saharan African',
                        'African American or Afro-Caribbean',
                        'African unspecified') ~ 'African',

    # Asian
    study_ancest %in% c('East Asian, Asian unspecified',
                        'South Asian, East Asian ',
                        'South Asian, South East Asian',
                        'South Asian, South East Asian, East Asian',
                        'South East Asian, East Asian',
                        'South East Asian, South Asian, East Asian',
                        'South Asian, South East Asian, East Asian, Asian unspecified',
                        'South East Asian, East Asian, South Asian',
                        'East Asian, South Asian, South East Asian',
                        'East Asian, South East Asian, South Asian, Asian unspecified',
                        'South Asian',
                        'South East Asian',
                        'South Asian, East Asian',
                        'South Asian, Asian unspecified',
                        'Central Asian, South Asian',
                        'East Asian, South Asian',
                        'Central Asian',
                        'East Asian',
                        'Asian unspecified') ~'Asian',

    # Middle eastern
    study_ancest %in% c('Greater Middle Eastern (Middle Eastern, North African or Persian)',
                        'Middle Eastern') ~'Middle Eastern',

    # Oceanic
    study_ancest %in% c('Aboriginal Australian',
                        'Oceanian') ~'Oceanic',

    # Hispanic/Latin American
    study_ancest %in% "Hispanic or Latin American" ~'Hispanic/Latin American',

    # Other
    study_ancest %in% c('Other',
                        'Other, NR',
                        'NR, Other',
                        'other',
                        'Other admixed ancestry',
                        'Native American') ~ "Other",

    # Not reported
    study_ancest %in% c("NR") ~ "Not reported",

    # Multiple
    grepl(", ", study_ancest) ~ 'Multiple',

    TRUE~study_ancest
  )



}


# Set up custom theme for ggplots
custom_theme <-
  list(
    theme_bw(base_size = 20) +
      theme(
        panel.border = element_blank(),
        axis.line = element_line(),
        #text = element_text(size = 16),
        legend.position = "right",
        strip.background = element_blank()
        #axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  )


# Add final totals
add_final_totals <- function(ancestry_df, window_months = 6){

  # get date of last publication
  max_date = ancestry_df$DATE |> max()

  # get date of last publication for this ancestry group

  # for each 6 months between final max date and the last date for that ancestry group, add a row with the final total

  ancestry_df_totals= ancestry_df |>
    dplyr::group_by(ancestry_group) |>
    dplyr::summarise(ancest_cumsum = max(ancest_cumsum), .groups = "drop") |>
    dplyr::mutate(DATE = max_date)

  ancestry_df = bind_rows(ancestry_df, ancestry_df_totals)

  return(ancestry_df)



}

add_final_totals <- function(ancestry_df, window_months = 6) {

  # Get date of the final publication across all ancestry groups
  max_date <- max(ancestry_df$DATE, na.rm = TRUE)

  # Get the last publication date and final cumulative total for each
  # ancestry group
  ancestry_df_totals <- ancestry_df |>
    dplyr::group_by(ancestry_group) |>
    dplyr::summarise(
      last_date = max(DATE, na.rm = TRUE),
      ancest_cumsum = max(ancest_cumsum, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      DATE = list(
        seq(
          from = last_date %m+% lubridate::months(window_months),
          to = max_date,
          by = paste(window_months, "months")
        )
      )
    ) |>
    tidyr::unnest(DATE) |>
    dplyr::ungroup() |>
    dplyr::select(ancestry_group, DATE, ancest_cumsum)

  # Add the rows containing the final totals
  dplyr::bind_rows(
    ancestry_df,
    ancestry_df_totals
  )
}

# Add final totals, for reversed axis:
add_final_totals_reversed <- function(ancestry_df){

  min_date = ancestry_df |>
             ungroup() |>
             pull(DATE) |>
             min()

  ancestry_df_totals = ancestry_df |>
    dplyr::group_by(ancestry_group) |>
    slice_max(order_by = ancest_cumsum, n = 1, with_ties = F) |>
    #dplyr::summarise(ancest_cumsum = max(ancest_cumsum), .groups = "drop") |>
    dplyr::mutate(DATE = min_date) |>
    dplyr::ungroup()

  ancestry_df = bind_rows(ancestry_df,
                          ancestry_df_totals)

  max_date = ancestry_df |>
    ungroup() |>
    pull(DATE) |>
    max()

  #max_date = lubridate::rollforward(max_date)

  # add a O tally
  start_tally =
  ancestry_df |>
    select(ancestry_group) |>
    distinct() |>
    mutate(ancest_cumsum = 0,
           DATE = max_date)

  ancestry_df = bind_rows(start_tally,
                          ancestry_df)


  return(ancestry_df)

}


add_final_totals <- function(ancestry_df,
                             window_months = 12) {

  max_date <- max(ancestry_df$DATE,
                  na.rm = TRUE)

  dates_to_add_total_tally_to <-
  ancestry_df |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(DATE)) |>
    dplyr::group_by(ancestry_group) |>
    dplyr::slice_max(DATE, with_ties = FALSE) |>
    select(ancestry_group, DATE) |>
    mutate(
      date_windows =
      stringr::str_flatten(
      c(
      seq(ymd(DATE),
          ymd(max_date),
          by= paste0(window_months, " months")),
      max_date),
     collapse = ";")
    ) |>
    tidyr::separate_longer_delim(date_windows,
                                 delim = ";") |>
    select(-DATE) |>
    rename(DATE = date_windows) |>
    ungroup() |>
    distinct()

  total_tally <- ancestry_df |>
    dplyr::group_by(ancestry_group) |>
    dplyr::slice_max(ancest_cumsum,
                     n = 1,
                     with_ties = FALSE) |>
    ungroup() |>
    dplyr::select(ancestry_group, ancest_cumsum)

  totals <-
  left_join(
  dates_to_add_total_tally_to,
  total_tally,
  by = "ancestry_group"
  ) |>
    dplyr::select(ancestry_group,
                  DATE,
                  ancest_cumsum) |>
    dplyr::mutate(DATE = lubridate::ymd(DATE))

  ancestry_df <-
  dplyr::bind_rows(dplyr::ungroup(ancestry_df) |>
                   dplyr::mutate(DATE = lubridate::ymd(DATE)),
                   totals)

  return(ancestry_df)
}
