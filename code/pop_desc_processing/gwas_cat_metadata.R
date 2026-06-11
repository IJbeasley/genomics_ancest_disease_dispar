# Functions for extracting and grouping
# population descriptors in GWAS Catalog
# sample description metadata

get_ancestry_from_sample_desc = function(sample_description){
  # Possible Ancestry/Description Terms in Sample Description Column of
  # GWAS Catalog Ancestry Metadata

  # sorted in groups of similar terms,
  # roughly by length of matched terms
  multi_group_ancestry_terms <- c(

    # Start with many groups
    'European, African, African American, South East Asian, East Asian, American Indian\\/Alaskan Native, Mixed race, Arabic\\/North African, Central\\/South Asian or Japanese Asian',
    '^European, African American, Hispanic, Asian, Pacific Islander and other(?= ancestry)',
    'European ancestry, African American or Afro-Caribbean, Hispanic or Latin American',

    'Asian, European, African and other(?= ancestry)',
    'European, African, Asian and unknown(?= ancestry)',
    'South Asian, Afro-Caribbean or unknown(?= ancestry)',
    'European, African American, South Asian and Hispanic',

    # European and/or African / African American
    '(?<![Ff]innish )European and African(?= ancestry)',
    '(?<![Ff]innish )European or African American',

    # European and Asian
    "European, South Asian or East Asian",
    'European, Hispanic and other(?= ancestry)',
    'European ancestry, East Asian ancestry, unknown',
    '(?<![Ff]innish )European and South Asian(?= ancestry)',
    "(?<!Han )Chinese or European(?= ancestry)",
    '(?<![Ff]innish )European or East Asian(?= ancestry)',
    '(?<![Ff]innish )European and East Asian(?= ancestry)',

    # European and unknown ancestry
    '(?<![Ff]innish )European ancestry, unknown(?= cases)',
    '(?<![Ff]innish )European or unknown(?= ancestry)',
    '(?<![Ff]innish )European and unknown(?= ancestry)',
    '(?<![Ff]innish )European or uknown(?= ancestry)',

    # European and other
    'Ashkenazi Jewish',
    '(?<![Ff]innish )European and other(?= ancestry)',
    '(?<![Ff]innish )European ance other(?= ancestry)',


    'African and Asian(?= ancestry)',
    'Sub-Saharan African ancestry and African ancestry',
    'East Asian, Asian, and other(?= ancestry)'
  )

  # sorted in groups of similar terms,
  # roughly by length of matched terms
  single_ancestry_terms = c(
    # 'African American or Afro-Caribbean, African etc.'
    'African(?:-|\\s)American or Afro-Caribbean',
    'African or African(?:-|\\s)American', # African or African American
    'African American, African(?= ancestry)',
    '(?<!or )African(?:-|\\s)American(?! or Afr)',
    '(?<!or )Afro-Caribbean(?! or)',
    'African unspecified',
    '(?<!South )(?<!North )(?<!West )(?<!Sub-Saharan )African(?!-|\\s)(?! or)',
    'South African(?! or)',
    'West African(?! or)',
    'Sub-Saharan African(?! or)(?! and)',
    'Jamaican',
    'Ethiopian',
    'Black or Black British',
    'Black(?! or)(?! British)',

    # Oceanian
    'Fijian Indian',
    'Filipino(?= ancestry)',
    'Oceanian',
    'Martu Australian Aboriginal(?= ancestry)',
    'Aboriginal Australian',

    # American Native, Latino, American Admixed
    'American Indian\\/Alaskan Native',
    'American Indian(?!\\/)',
    '(?<!Indigenous )Mexican and other Latin American',
    'Indigenous Mexican(?! and)(?! or)',
    'Mexican American',
    'Admixed American',
    '(?<! Indigenous )Mexican(?! American)(?! and)',
    'Hispanic\\/Latino',
    'Hispanic American',
    'Hispanic or Latin American',
    'Hispanic or Latino',
    'Hispanic(?! or Latin)(?! American)(?!/)',
    '(?<! other)(?<!or )Latin American',
    '(?<!\\% )Native American',
    '(?<!\\% )Native Hawaiian',
    'Pima Indian',
    'Costa Rican',


    # Asian, East Asian etc.
    'Asian or Asian British',
    'Central/South Asian(?! unspecified)',
    'Central Asian(?! unspecified)',
    'South East Asian',
    '(?<!South )East Asian',
    "(?<!East )(?<!South )(?<!Central )Asian(?! unspecified)(?!or)",
    "(?<!East )(?<!South )(?<!Central )Asian(?= unspecified)",
    'Han(?:-|\\s)Chinese',
    "(?<!Han )Southern Chinese", # but not Han Chinese
    '(?<!Han )(?<!Southern )Chinese',
    'Japanese American',
    'Japanese(?! American)',
    'Oriental(?= ancestry)',
    'Pakistani(?= ancestry)',
    '(?<![Ww]hite )(?<![Bb]lack )(?<![Aa]sian )British',
    '(?<!American )(?<!Fijian )Northern Indian(?! Asian)',
    'Indian Asian',
    'Asian Indian',
    '(?<!American )(?<!Fijian )(?<!Northern )(?<!Pima )Indian(?! Asian)',
    'Korean',
    'South Asian(?! or)(?! and)',
    'Malaysian',
    'Malay\\b',
    'Taiwanese',

    # Middle eastern
    'Greater Middle Eastern \\(Middle Eastern, North African or Persian\\)',
    '(?<!Greater )(?<!\\()Middle Eastern(?! \\()',
    'Greater Middle Eastern(?= ancestry)',
    'Saudi Arabian',
    'Saudi Arab(?= ancestry)',
    'Lebanese',
    'Arabic\\/North African(?= ancestry)',
    'Arab\\b',

    # European ancestry
    'Findland founder',
    'Finnland founder',
    '[Nn]on-[Ff]innish European',
    '(?<![Nn]on-)[Ff]innish',
    'French [Cc]anadian',
    'French(?= ancestry)',
    '(?<![Ff]innish )(?<!with )European',
    'Greek(?= ancestry)',
    'Old Order Amish',
    '(?<!Old Order )Amish',
    'Celtic',
    'German',
    'Italian',
    'Mylopotamos',
    'Polish(?= ancestry)',
    'Scottish',
    'Spanish',
    'Slavic',
    'Swedish',

    # Other, unknown, or admixed unspecified
    '(?<!and )[Oo]ther(?:-|\\s)[Aa]dmixed(?= ancestry)',
    'mixed and [Oo]ther(?= ancestry)',
    '[Oo]ther(?= ancestry)',
    'NR',
    'unknown',
    "White(?= ancestry)",
    'Mixed race'
  )

  ancestry_terms <-   c(
    multi_group_ancestry_terms,
    single_ancestry_terms
  )

  # try match one term at a time
  vapply(sample_description, function(desc){
    if (is.na(desc) || !nzchar(desc)) return(NA_character_)

    remaining <- desc
    found <- character(0)

    for (term in ancestry_terms){

      hit <- stringr::str_extract(remaining,
                                  term)
      if (!is.na(hit)){
        found <- c(found, hit)

        remaining <- stringr::str_remove_all(remaining,
                                             term)
      }
    }

    if (length(found) == 0) return('NR')

    found <- paste(found,
                  collapse = "; ")

    return(found)
    # return in the order the terms appear in the original description
    #   found <- found[order(vapply(found, function(t)
    #     str_locate(desc, t)[, "start"], numeric(1)))]
    #   paste(found, collapse = ", ")
    # }, character(1), USE.NAMES = FALSE)
  },
  character(1),
  USE.NAMES = FALSE)
}



# code adapted from https://github.com/armartin/prs_disparities/blob/master/gwas_disparities_time.R
group_pop_labels <- function(study_desc) {

  study_desc <- stringr::str_to_title(study_desc)

  pop_lab <- dplyr::case_when(

    #################### African American or Afro-Caribbean + other ancestry groups ###############

    # "African American or Afro-Caribbean, African unspecified"
    study_desc %in% c('African, African American',
                      'African American, African',
                      'African Or African American'
    ) ~ "African American or Afro-Caribbean, African unspecified",

    #  African American or Afro-Caribbean, Asian unspecified, European, Hispanic or Latin American, Oceanian, Other
    study_desc %in% c('European, African American, Hispanic, Asian, Pacific Islander And Other') ~
      'African American or Afro-Caribbean, Asian unspecified, European, Hispanic or Latin American, Oceanian, Other',

    # African American or Afro-Caribbean, NR, South Asian
    study_desc %in% c('South Asian, Afro-Caribbean Or Unknown') ~
      'African American or Afro-Caribbean, NR, South Asian',

    # "African American or Afro-Caribbean, European"
    study_desc %in% c('European Or African American') ~
      "African American or Afro-Caribbean, European",

    # "African American or Afro-Caribbean, European, Hispanic or Latin American"
    study_desc %in% c('European, African American, South Asian And Hispanic',
                      'European Ancestry, African American Or Afro-Caribbean, Hispanic Or Latin American') ~
      "African American or Afro-Caribbean, European, Hispanic or Latin American",


    #################### African unspecified + other ancestry groups ###############

    # "African unspecified, Asian unspecified"
    study_desc %in% c('African And Asian') ~
      'African unspecified, Asian unspecified',

    # "African unspecified, Asian unspecified, European, NR"
    study_desc %in% c('Asian, European, African And Other',
                      'European, African, Asian And Unknown') ~
      "African unspecified, Asian unspecified, European, NR",

    # African unspecified, European
    study_desc %in% c('European And African') ~
      'African unspecified, European',

    # African unspecified, Sub-Saharan African
    study_desc %in% c('Sub-Saharan African Ancestry And African Ancestry') ~
      "African unspecified, Sub-Saharan African",

    #################### Asian unspecified + other ancestry groups ###############

    # Asian unspecified, East Asian, Other
    study_desc %in% c('East Asian, Asian, And Other') ~
      "Asian unspecified, East Asian, Other",

    #################### Central Asian + other ancestry groups ###############

    # "Central Asian, South Asian"
    study_desc %in% c('Central/South Asian') ~
      "Central Asian, South Asian",

    #################### East Asian + other ancestry groups ###############

    # "East Asian, European"
    study_desc %in% c('Chinese Or European',
                      'European And East Asian',
                      'European Or East Asian') ~  "East Asian, European",

    # "East Asian, European, NR"
    study_desc %in% c('European Ancestry, East Asian Ancestry, Unknown') ~
      "East Asian, European, NR",

    # "East Asian, European, South Asian"
    study_desc %in% c('European, South Asian Or East Asian') ~
      "East Asian, European, South Asian",


    #################### European + other ancestry groups ###############

    # European, NR
    study_desc %in% c('European Or Unknown',
                      'European And Unknown',
                      'European Or Uknown',
                      'European Ancestry, Unknown') ~
      'European, NR',

    # European, Other
    study_desc %in% c('European Ance Other',
                      'European And Other') ~
      'European, Other',

    # European, South Asian
    study_desc %in% c('European And South Asian') ~
      'European, South Asian',

    # European, Hispanic or Latin American, Other
    study_desc %in% c('European, Hispanic And Other') ~
      'European, Hispanic or Latin American, Other',


    ############### Single broad ancestry groups ################

    # African-American or Afro-Caribbean
    study_desc %in% c('Afro-Caribbean',
                      'Afro Caribbean',
                      'African American',
                      'African-American',
                      'African American Or Afro-Caribbean',
                      'African-American Or Afro-Caribbean',
                      'African American Or Afro Caribbean',
                      "Jamaican")
    ~ "African-American or Afro-Caribbean",

    # African unspecified
    study_desc %in% c('African',
                      'African Unspecified') ~
      "African unspecified",

    # Asian unspecified
    study_desc %in% c('Asian',
                      'Oriental',
                      'Asian Unspecified') ~ "Asian unspecified",

    # Central asian
    study_desc %in% c('Central Asian') ~
      "Central Asian",

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

    # European
    study_desc %in% c('Amish',
                      'British',
                      'Celtic',
                      'European',
                      'Non-Finnish European',
                      'Finnish',
                      'Findland founder',
                      'Finnland founder',
                      'French Canadian',
                      'French',
                      'Greek',
                      'German',
                      'Italian',
                      'Mylopotamos',
                      'Old Order Amish',
                      'Polish',
                      'Scottish',
                      'Slavic',
                      'Spanish',
                      'Swedish',
                      'White'
                      ) ~ 'European',

    #  Greater Middle Eastern
    study_desc %in% c('Greater Middle Eastern (Middle Eastern, North African Or Persian)',
                      'North African',
                      'Saudi Arab',
                      'Saudi Arabian',
                      'Lebanese',
                      'Persian',
                      'Middle Eastern',
                      'Arab') ~ 'Greater Middle Eastern (Middle Eastern, North African or Persian)',

    # Sub-Saharan African
    study_desc %in% c('South African',
                      'West African',
                      'Sub-Saharan African',
                      'Ethiopian') ~ "Sub-Saharan African",

    # South Asian
    study_desc %in% c('Indian',
                      'Indian Asian',
                      'Asian Indian',
                      'Pakistani',
                      'South Asian') ~ "South Asian",


    # South East Asian
    study_desc %in% c('South East Asian',
                      'Malaysian',
                      'Malay',
                      'Vietnam',
                      'Vietnamese') ~ "South East Asian",


    # Hispanic or Latin American
    study_desc %in% c('Hispanic',
                      'Hispanic/Latin American',
                      'Latin American/Hispanic',
                      'Hispanic/Latino',
                      'Hispanic American',
                      'Mexican',
                      'Mexican American',
                      'Costa Rican',
                      'Hispanic Or Latin American',
                      'Latin American') ~ 'Hispanic or Latin American',

    # Native American
    study_desc %in% c("Native American",
                      "Alaskan Native",
                      'American Indian'
                      'American Indian/Alaskan Native',
                      'Pima Indian'
    ) ~ "Native American",

    # Not reported
    study_desc %in% c("NR",
                      'Nr',
                      "Unknown") ~ "NR",

    # Oceanian
    study_desc %in% c("Native Hawaiian",
                      'Pacific Islander'
    ) ~ "Oceanian",

    # Other
    study_desc %in% c('Other') ~ "Other",

    # Other Admixed
    study_desc %in% c('Other-Admixed',
                      'Admixed American',
                      'Other Admixed') ~
      "Other admixed ancestry",

    TRUE~"Nothing"
  )

  return(pop_lab)
}

