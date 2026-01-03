# Functions + data for custom plotting:

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


# code adapted from https://github.com/armartin/prs_disparities/blob/master/gwas_disparities_time.R

group_ancestry_fn = function(study_ancest){

  case_when(

    # European
    study_ancest %in% c('European') ~ 'European',

    # African
    study_ancest %in% c('Sub-Saharan African, African American or Afro-Caribbean',
                        'African unspecified, African American or Afro-Caribbean',
                        'African American or Afro-Caribbean, African unspecified',
                        'Sub-Saharan African, African unspecified',
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
    study_ancest == 'Greater Middle Eastern (Middle Eastern, North African or Persian)' ~'Middle Eastern',

    # Oceanic
    study_ancest %in% c('Aboriginal Australian',
                        'Oceanian') ~'Oceanic',

    # Hispanic/Latin American
    study_ancest %in% "Hispanic or Latin American" ~'Hispanic/Latin American',

    # Other
    study_ancest %in% c('Other',
                        'Other, NR',
                        'NR, Other',
                        'Other admixed ancestry',
                        'Native American') ~ "Other",

    # Not reported
    study_ancest %in% "NR" ~ "Not reported",

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
add_final_totals <- function(ancestry_df){

  max_date = ancestry_df$DATE |> max()

  ancestry_df_totals= ancestry_df |>
    dplyr::group_by(ancestry_group) |>
    dplyr::summarise(ancest_cumsum = max(ancest_cumsum), .groups = "drop") |>
    dplyr::mutate(DATE = max_date)

  ancestry_df = bind_rows(ancestry_df, ancestry_df_totals)

  return(ancestry_df)



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
