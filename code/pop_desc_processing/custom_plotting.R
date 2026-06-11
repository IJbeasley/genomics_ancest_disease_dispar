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

library(stringr)

get_ancestry_from_sample_desc = function(sample_description){
  # Possible Ancestry/Description Terms in Sample Description Column of
  # GWAS Catalog Ancestry Metadata

  # sorted in alphabetical order
  ancestry_terms = c(
    'Martu Australian Aboriginal(?= ancestry)',
    'Aboriginal Australian',
    'Greater Middle Eastern \\(Middle Eastern, North African or Persian\\)',

    # Multi-group terms:
    '^European, African American, Hispanic, Asian, Pacific Islander and other(?= ancestry)',
    # 'European, African American, East Asian, South Asian or Hispanic',
    # 'European, African American, East Asian, South Asian, mixed Asian or Latin American',
    # 'European ancestry, African American or Afro-Caribbean, East Asian ancestry, Hispanic or Latin American, South Asian',
    # 'African, African American, East Asian, South Asian, European, Middle Eastern, admixed American, other or other admixed',
    # 'African American, Asian, Native Hawaiian or Pacific Islander and unknown',
    'African and Asian(?= ancestry)',

    'Sub-Saharan African ancestry and African ancestry',

    'Asian, European, African and other(?= ancestry)',
    'European, African, Asian and unknown(?= ancestry)',
    'South Asian, Afro-Caribbean or unknown(?= ancestry)',
    'European, African American, South Asian and Hispanic',
    'European ancestry, African American or Afro-Caribbean, Hispanic or Latin American',
    'European, African, African American, South East Asian, East Asian, American Indian\\/Alaskan Native, Mixed race, Arabic/North African, Central/South Asian or Japanese Asian',

    # European and African
    '(?<![Ff]innish )European and African(?= ancestry)',

    "European, South Asian or East Asian",

    # European, Hispanic and other
    'European, Hispanic and other(?= ancestry)',

    # European, East Asian, unknown
    'European ancestry, East Asian ancestry, unknown',

    # # European or African American
    '(?<![Ff]innish )European or African American',
    # European and South Asian
    '(?<![Ff]innish )European and South Asian(?= ancestry)',

    # East Asian or European
    "(?<!Han )Chinese or European(?= ancestry)",
    '(?<![Ff]innish )European or East Asian(?= ancestry)',
    '(?<![Ff]innish )European and East Asian(?= ancestry)',

    'East Asian, Asian, and other(?= ancestry)',
    # 'African American or Afro-Caribbean',
    # 'African American or African',
    'African(?:-|\\s)American or Afro-Caribbean',
    'African or African(?:-|\\s)American', # African or African American
    'African American, African(?= ancestry)',


    '(?<![Aa]frican(?:-|\\s)American or)Afro-Caribbean',
    '(?<!or )African(?:-|\\s)American(?! or Afr)',
    '(?<!or )Afro-Caribbean(?! or)',
    'African unspecified',
    '(?<!South )(?<!North )(?<!West )(?<!Sub-Saharan )African(?!-|\\s)(?! or)',
    'South African(?!or)',
    'West African(?! or)',
    'American Indian\\/Alaskan Native',
    'American Indian(?!\\/)',
    'Arabic\\/North African(?= ancestry)',
    'Arab\\b',
    'Admixed American',
    'Black(?! or)(?! British)',
    'Black or Black British',
    '(?<![Ww]hite )(?<![Bb]lack )(?<![Aa]sian )British',
    '(?<!South )African(?![-\\s]American)',
    'Asian or Asian British',
    "(?<!East )(?<!South )(?<!Central)Asian(?= unspecified)",
    "(?<!East )(?<!South )(?<!Central)Asian(?! unspecified)(?!or)",
    "(?<!East )(?<!South )(?<!Indian )Asian",
    'Central/South Asian',
    'Central Asian',
    'Celtic',
    'Costa Rican',
    'German',

    '(?<!South )East Asian',
    'Ethiopian',
    'Filipino(?= ancestry)',
    '(?<![Nn]on-)[Ff]innish',
    'French [Cc]andian',
    'French(?= ancestry)',
    'Fijian Indian',

    # European and other
    'Ashkenazi Jewish',
    'Findland founder',
    '(?<![Ff]innish )European and other(?= ancestry)',
    '(?<![Ff]innish )European ance other(?= ancestry)',

    # European and unknown ancestry
    '(?<![Ff]innish )European ancestry, unknown(?= cases)',
    '(?<![Ff]innish )European or unknown(?= ancestry)',
    '(?<![Ff]innish )European and unknown(?= ancestry)',
    '(?<![Ff]innish )European or uknown(?= ancestry)',
    # '(?<![Ff]innish )European ancestry East Asian',
    '(?<![Ff]innish )(?<!with )European',
    '[Nn]on-[Ff]innish European',
    'Greek(?= ancestry)',
    'Han(?:-|\\s)Chinese',
    '(?<!Han )(?<!Southern )Chinese',
    "(?<!Han )Southern Chinese", # but not Han Chinese
    'Hispanic\\/Latino',
    'Hispanic American',
    'Hispanic(?! or Latin)(?! American)(?!/)',
    'Hispanic or Latin American',
    'Hispanic or Latino',
    'Indian Asian',
    'Asian Indian',
    'Italian',
    'Pima Indian',
    '(?<!American )(?<!Fijian )Northern Indian(?! Asian)',
    '(?<!American )(?<!Fijian )(?<!Northern )(?<!Pima )Indian(?! Asian)',
    'Japanese American',
    'Japanese(?! American)',
    'Jamaican',
    'Korean',
    'Lebanese',
    '(?<! other)(?<!or )Latin American',
    'Malay\\b',
    'Indigenous Mexican',
    'Mexican American',
    '(?<! Indigenous )Mexican(?! American)(?! and)',
    'Mexican and other Latin American',
    'Mixed race',
    'Mylopotamos',
    '(?<!Greater )(?<!\\()Middle Eastern(?! \\()',
    'Greater Middle Eastern(?= ancestry)',
    '(?<!\\% )Native American',
    '(?<!\\% )Native Hawaiian',
    'Oceanian',
    'Oriental(?= ancestry)',
    'Old Order Amish',
    '(?<!Old Order )Amish',
    '(?<!and )[Oo]ther(?:-|\\s)[Aa]dmixed(?= ancestry)',
    'mixed and [Oo]ther(?= ancestry)',
    '[Oo]ther(?= ancestry)',
    'Pakistani(?= ancestry)',
    'Polish(?= ancestry)',
#    '[Oo]ther(?! [Aa]dmixed)(?! [Aa]ncestry)',
    'Scottish',
    'Saudi Arab(?= ancestry)',
    'Saudi Arabian',
    'Sub-Saharan African',
    'South East Asian',
    'Spanish',
    'Slavic',
    'Swedish',
    'South Asian',
    'Taiwanese',
    'NR',
    'unknown',
    "White(?= ancestry)"
  )

  vapply(sample_description, function(desc){
    if (is.na(desc) || !nzchar(desc)) return(NA_character_)
    remaining <- desc
    found <- character(0)
    # for (term in ancestry_terms){
    #   if (str_detect(remaining, term)){
    #     found <- c(found, term)
    #     # strip the matched term so its substrings aren't re-matched
    #     remaining <- str_remove_all(remaining, term)
    #   }
    # }
    for (term in ancestry_terms){
      hit <- str_extract(remaining, term)
      if (!is.na(hit)){
        found <- c(found, hit)
        remaining <- str_remove_all(remaining, term)
      }
    }

    if (length(found) == 0) return('NR')

    found = paste(found, collapse = "; ")

    return(found)
    # return in the order the terms appear in the original description
  #   found <- found[order(vapply(found, function(t)
  #     str_locate(desc, t)[, "start"], numeric(1)))]
  #   paste(found, collapse = ", ")
  # }, character(1), USE.NAMES = FALSE)
  }, character(1), USE.NAMES = FALSE)
}
# code adapted from https://github.com/armartin/prs_disparities/blob/master/gwas_disparities_time.R
group_pop_labels <- function(study_desc) {

  case_when(

    # European
    study_desc %in% c('European',
                      'British',
                       'White',
                      'Finnish',
                      'German',
                      'Italian',
                       'Celtic',
                       'Slavic',
                       'non-Finnish European',
                       'Swedish',
                       'Spanish',
                      'Amish',
                      'Old Older Amish',
                        'Scottish') ~ 'European',

    # # European, NR
    study_desc %in% c('European or unknown',
                      'European and unknown',
                      'European or uknown',
                      'European ancestry, unknown') ~ 'European, NR',

    # # European, Other
    study_desc %in% c('European ance other',
                      'European and other') ~ 'European, Other',

    # European, South Asian
    study_desc %in% c('European and South Asian',
                      'European and other') ~ 'European, South Asian',

    # European, Hispanic or Latin American, Other
    study_desc %in% c('European, Hispanic and other') ~
      'European, Hispanic or Latin American, Other',

    # "East Asian, European"
    study_desc %in% c('Chinese or European',
                      'European and East Asian',
                      'European or East Asian') ~  "East Asian, European",

    # "East Asian, European, NR"
    study_desc %in% c('European ancestry, East Asian ancestry, unknown') ~
      "East Asian, European, NR",

    # "East Asian, European, South Asian"
    study_desc %in% c('European, South Asian or East Asian') ~
      "East Asian, European, South Asian",

    # "Central Asian, South Asian"
    study_desc %in% c('Central/South Asian') ~
      "Central Asian, South Asian",

    # Asian unspecified, East Asian, Other
    study_desc %in% c('East Asian, Asian, and other') ~
      "Asian unspecified, East Asian, Other",

    # African unspecified, European
    study_desc %in% c('European and African') ~ 'African unspecified, European',

    # "African unspecified, Asian unspecified"
    study_desc %in% c('African and Asian') ~ 'African unspecified, Asian unspecified',

    # African American or Afro-Caribbean, NR, South Asian
    study_desc %in% c('South Asian, Afro-Caribbean or unknown') ~ 'African American or Afro-Caribbean, NR, South Asian',

    # "African American or Afro-Caribbean, African unspecified"
    study_desc %in% c('African, African American',
                      'African American, African',
                      'African or African American'
                      ) ~ "African American or Afro-Caribbean, African unspecified",

    #  African American or Afro-Caribbean, Asian unspecified, European, Hispanic or Latin American, Oceanian, Other
    study_desc %in% c('European, African American, Hispanic, Asian, Pacific Islander and other') ~
    'African American or Afro-Caribbean, Asian unspecified, European, Hispanic or Latin American, Oceanian, Other',

    # "African American or Afro-Caribbean, European"
    study_desc %in% c('European or African American') ~  "African American or Afro-Caribbean, European",

    # "African American or Afro-Caribbean, European"
    study_desc %in% c('European, African American, South Asian and Hispanic',
                      'European ancestry, African American or Afro-Caribbean, Hispanic or Latin American') ~  "African American or Afro-Caribbean, European, Hispanic or Latin American",

    # "African unspecified, Asian unspecified, European, NR"
    study_desc %in% c('Asian, European, African and other',
                      'European, African, Asian and unknown') ~
      "African unspecified, Asian unspecified, European, NR",

    # African unspecified, Sub-Saharan African
    # 'Sub-Saharan African ancestry and African ancestry'
    study_desc %in% c('Sub-Saharan African ancestry and African ancestry') ~
      "African unspecified, Sub-Saharan African",


    # African-American or Afro-Caribbean
    study_desc %in% c('Afro-Caribbean',
                      'Afro Caribbean',
                      'African American',
                      'African-American',
                      'African American or Afro-Caribbean',
                      'African-American or Afro-Caribbean',
                      'African American or Afro Caribbean',
                      "Jamaican")
    ~ "African-American or Afro-Caribbean",

    # Sub-Saharan African
    study_desc %in% c('South African',
                      'West African',
                      'Sub-Saharan African',
                      'Ethiopian') ~ "Sub-Saharan African",

    # African unspecified
    study_desc %in% c('African',
                      'African unspecified') ~ "African unspecified",

    # South East Asian
    study_desc %in% c('South East Asian',
                      'Malay',
                      'Vietnam',
                      'Vietnamese') ~ "South East Asian",

    # South Asian
    study_desc %in% c('Indian',
                      'Indian Asian',
                      'Asian Indian',
                      'Pakistani',
                      'South Asian') ~ "South Asian",

    # East Asian
    study_desc %in% c("Japan",
                      "Japanese",
                      "Korea",
                      "Korean",
                      "Chinese",
                      "Han-Chinese",
                      'Han Chinese',
                      "Taiwan",
                      'Taiwanese',
                      "East Asian"
                      ) ~ "East Asian",

    # Asian unspecified
    study_desc %in% c('Asian',
                      'Oriental',
                      'Asian unspecified') ~ "Asian unspecified",

    study_desc %in% c('Central Asian') ~ "Central Asian",

    # Hispanic or Latin American
    study_desc %in% c('Hispanic',
                      'Hispanic/Latin American',
                      'Latin American/Hispanic',
                      'Hispanic/Latino',
                      'Hispanic American',
                      'Mexican',
                      'Mexican American',
                      'Costa Rican',
                      'Hispanic or Latin American',
                      'Latin American') ~ 'Hispanic or Latin American',

    # Native American
    study_desc %in% c("Native American",
                      "Alaskan Native",
                      'American Indian'
                      ) ~ "Native American",

    # Oceanian
    study_desc %in% c("Native Hawaiian",
                      'Pacific Islander'
    ) ~ "Oceanian",


    # Not reported
    study_desc %in% c("NR",
                       "unknown") ~ "NR",

    #  Greater Middle Eastern
    study_desc %in% c('Greater Middle Eastern (Middle Eastern, North African or Persian)',
                        'North African',
                      'Saudi Arab',
                      'Saudi Arabian',
                        'Lebanese',
                        'Persian',
                        'Middle Eastern',
                        'Arab') ~ 'Greater Middle Eastern (Middle Eastern, North African or Persian)',

    # Other
    study_desc %in% c('Other',
                        'other') ~ "Other",

    # Other Admixed
    study_desc %in% c('other-admixed',
                      'Admixed American',
                      'Other admixed') ~ "Other admixed ancestry",

    TRUE~"Nothing"
  )
}




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
