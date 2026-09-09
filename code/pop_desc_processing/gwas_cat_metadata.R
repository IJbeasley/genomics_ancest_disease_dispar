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
    #'European, African, African American, South East Asian, East Asian, American Indian\\/Alaskan Native, Mixed race, Arabic\\/North African, Central\\/South Asian or Japanese Asian',
    '^European, African American, Hispanic, Asian, Pacific Islander and other(?= ancestry)',
    #'European ancestry, African American or Afro-Caribbean, Hispanic or Latin American',
    'European, South Asian, East Asian, Hispanic or African American(?= individuals)',
    'European ancestry, East Asian ancestry, South Asian ancestry, African American, Hispanic(?= (cases|controls))',
    #'European ancestry, South East Asian ancestry, South Asian ancestry, African ancestry and South African(?= ancestry)',
    #'European, Asian, African, Arabic, Native American, Pacific Islander and other(?= ancestry)',
    #'European, African American, East Asian, South Asian, mixed Asian or Latin American',
    #'European, African American, East Asian, South Asian or Hispanic',
    #'African American, Asian, Native Hawaiian or Pacific Islander and unknown(?= ancestry)',
    'African American, East Asian, Hispanic/Latino or South Asian(?= (cases|controls))',
    #'East Asian, South Asian, Middle Eastern, Hispanic and African American(?= ancestry)',

    #'Asian, European, African and other(?= ancestry)',
    #'European, African, Asian and unknown(?= ancestry)',
    #'South Asian, Afro-Caribbean or unknown(?= ancestry)',
    #'European, African American, South Asian and Hispanic',
    #'(?<![Ff]innish )European and South Asian(?= (ancestry|controls))',

    # European and/or African / African American
    '(?<![Ff]innish )European and African(?= ancestry)',
    '(?<![Ff]innish )European or African American',

    # European and Asian
    #"European, South Asian or East Asian",
    #'European, Hispanic and other(?= ancestry)',
    #'European ancestry, East Asian ancestry, unknown',
    "(?<!Han )Chinese or European(?= ancestry)",
    '(?<![Ff]innish )European or East Asian(?= ancestry)',
    #'(?<![Ff]innish )European and East Asian(?= ancestry)',
    #'(?<![Ff]innish )European ancestry, East Asian(?= ancestry)',

    # European and unknown ancestry
    '(?<![Ff]innish )European or unknown(?= ancestry)',
    '(?<![Ff]innish )European and unknown(?= ancestry)',
    '(?<![Ff]innish )European or uknown(?= ancestry)',
    #'European ancestry, unknown(?= (controls|cases))',

    # European and other
    '(?<![Ff]innish )European and other(?= ancestry)',
    '(?<![Ff]innish )European ance other(?= ancestry)',

    'African and Asian(?= ancestry)'
    #'Sub-Saharan African ancestry and African ancestry',
    #'Korean ancestry, Indian ancestry, Jamaican ancestry',
    #'East Asian, Asian, and other(?= ancestry)'
  )

  # sorted in groups of similar terms,
  # roughly by length of matched terms
  single_ancestry_terms = c(
    # 'African American or Afro-Caribbean, African etc.'
    'African American African(?= ancestry)',
    #'African, African American(?= ancestry)',
    'African(?:-|\\s)American or Afro-Caribbean',
    'African or African(?:-|\\s)American', # African or African American
    #'African American, African(?= ancestry)',
    '(?<!Afro-Caribbean or )African(?:-|\\s)American(?! or Afro-Cari)',
    '(?<!or )Afro-Caribbean(?! or Afr)',
    'African unspecified',
    '^African(?= cases)',
    '(?<!(South|North|West|Sub-[Ss]aharan) )African(?= (ancestry|and))',
    '(?<!(South|North|West|Sub-[Ss]aharan) )African(?!(-|\\s)|(or))',
    'South African Zulu(?= ancestry)',
    'South African(?! (or|Zulu))',
    'West African(?! or)',
    'Sub-[Ss]aharan African(?! (or|and))',
    'Jamaican',
    'Ethiopian',
    'Black or Black British',
    'Black(?! or)(?! British)',
    'Kenyan(?= ancestry)',
    '(?<!or )Zulu(?= ancestry)',

    # Oceanian
    'Fijian Indian',
    'Filipino(?= ancestry)',
    'Oceanian',
    'Martu Australian Aboriginal(?= ancestry)',
    'Aboriginal Australian',
    'Pacific Islander',

    # American Native, Latino, American Admixed
    'American Indian\\/Alaskan Native',
    'American Indian(?!\\/)',
    '(?<!Indigenous )Mexican and other Latin American',
    'Indigenous Mexican(?! (and|or))',
    'Mexican American',
    'Aboriginal Canadian',
    'Admixed American',
    'Mexican\\/Latino',
    '(?<! Indigenous )Mexican(?! American|and)',
    'Hispanic \\/ Latino American',
    'Hispanic\\/Latino American',
    'Hispanic\\/Latino',
    'Hispanic American',
    'Hispanic or Latin American',
    'Hispanic or Latino',
    '(?<!or )Latino',
    '(?<!non-)Hispanic(?! or Latin)(?! American)(?!/)',
    'Latin American',


    '(?<!\\% )Native American',
    '(?<!\\% )Native Hawaiian',
    'Pima Indian',
    'Costa Rican',
    'Brazilian(?= ancestry)',
    'Maya(?= ancestry)',
    'Chilean',


    # Asian, East Asian etc.
    'Asian or Asian British',
    'Central/South Asian(?! unspecified)',
    'Central Asian(?! unspecified)',
    'South East Asian',
    '(?<!South )East Asian',
    'Malaysian Chinese(?= ancestry)',
    'Indian Asian',
    'Asian Indian',
    'South Asian',
# 'South Asian(?! (or|and))',
#    "(?<!East )(?<!South )(?<!Central )Asian(?! unspecified)",
    "(?<!(East|South|Central|Japanese) )Asian(?=( unspecified|\\s| and|\\,|$))",
    'Han(?:-|\\s)Chinese',
    "(?<!Han )Southern Chinese", # but not Han Chinese
    '(?<!Han )(?<!Southern )Chinese',
    'Japanese American',
    'Japanese Asian',
    'Japanese(?! (American|Asian))',
    'Oriental(?= ancestry)',
    #'Pakistani and Bangladeshi(?= ancestry)',
    'Bangladeshi(?=(s|\\s))',
    'Pakistani(?= (ancestry|and|s))',
    '(?<!(American|Fijian) )Northern Indian(?! Asian)',
    '(?<!(American|Fijian|Northern|Pima) )Indian(?! Asian)',
    'Korean',
    'Malaysian',
    'Malay\\b',
    'Singaporean(?= ancestry)',
    'Taiwanese',
    'Thai(?= ancestry)',
    'Punjabi Sikh(?= ancestry)',
    'Sindhi(?= ancestry)',
    'Dravidian(?= ancestry)',

    # Middle eastern
    'Greater Middle Eastern \\(Middle Eastern, North African or Persian\\)',
    '(?<!Greater )(?<!\\()Middle Eastern(?! \\()',
    'Greater Middle Eastern(?= ancestry)',
    'Saudi Arabian',
    'Saudi Arab(?= ancestry)',
    'Lebanese',
    'Arabic\\/North African(?=( ancestry|,))',
    'Arabic',
    'Arab\\b',
    'Emirati(?= ancestry)',
    'Qatari(?= ancestry)',

    # European ancestry and related terms
    'Ashkenazi Jewish',
    'non\\-Hispanic White(?= ancestry)',
    'White British(?= ancestry)',
    '(?<!([Ww]hite|[Bb]lack|[Aa]sian) )British',
    'Findland founder',
    'Finland founder',
    'Indo-European',
    'Icelandic(?= ancestry)',
    'Irish',
    '[Nn]on-[Ff]innish European',
    '(?<![Nn]on-)[Ff]innish',
    'French [Cc]anadian',
    'French(?= ancestry)',
    '(?<!([Ff]innish|with) )European',
    '(?<![Ff]innish )(?<!with )Europen',
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
    '(?<!non-)Scandinavian(?= ancestry)',
    'non-Scandinavian(?= ancestry)',
    "\\bKorculan\\b",
    "\\bVis\\b",
    '\\bSplit\\b',

    # Other, unknown, or admixed unspecified
    '(?<!and )[Oo]ther(?:-|\\s)[Aa]dmixed(?= ancestry)',
    'mixed and [Oo]ther(?= ancestry)',
    '[Oo]ther(?= ancestry)',
    '[Oo]ther ancestries',
    '(?<=,)[Oo]ther',
    'NR',
    'unknown',
    "White(?= ancestry)",
    'Mixed race',
    'Greenlandic Inuit(?= ancestry)',
    'Greenlandic(?= ancestry)'
  )

  ancestry_terms <- single_ancestry_terms

  # ancestry_terms <-   c(
  #   multi_group_ancestry_terms,
  #   single_ancestry_terms
  # )

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

    # order ancestry terms by their order in the original string
    positions <- sapply(found, function(x) {
    stringr::str_locate(desc, fixed(x))[1]}
    )

    found <- found[order(positions)]

    found <- paste(found,
                  collapse = "; ")

    found <- str_remove_all(found,
                            " ancestry")

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
                      'African Or African American',
                      'African American African'
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
    ~ "African American or Afro-Caribbean",

    # African unspecified
    study_desc %in% c('African',
                      'African Unspecified',
                      'Black Or Black British',
                      'Black') ~
      "African unspecified",

    # Asian unspecified
    study_desc %in% c('Asian',
                      'Oriental',
                      'Asian or Asian British',
                      'Filipino',
                      'Asian Unspecified') ~ "Asian unspecified",

    # Central Asian
    study_desc %in% c('Central Asian') ~
      "Central Asian",

    # East Asian
    study_desc %in% c("Japan",
                      "Japanese",
                      "Japanese American",
                      'Japanese Asian',
                      "Korea",
                      "Korean",
                      'Southern Chinese',
                      "Chinese",
                      "Han-Chinese",
                      'Han Chinese',
                      "Taiwan",
                      'Taiwanese',
                      "East Asian",
                      'Malaysian Chinese'
    ) ~ "East Asian",

    # European
    study_desc %in% c('Amish',
                      'Ashkenazi Jewish',
                      'British',
                      'Irish',
                      'Celtic',
                      'European',
                      'Europen',
                      'Non-Finnish European',
                      'Non-Scandinavian',
                      'Non-Hispanic White',
                      'Finnish',
                      'Findland Founder',
                      'Finnland Founder',
                      'Finland Founder',
                      'French Canadian',
                      'French',
                      'Greek',
                      'German',
                      'Italian',
                      'Korculan',
                      'Mylopotamos',
                      'Old Order Amish',
                      'Polish',
                      'Scandinavian',
                      'Scottish',
                      'Slavic',
                      'Spanish',
                      'Swedish',
                      'Vis',
                      'Split',
                      'White',
                      'White British'
                      ) ~ 'European',

    #  Greater Middle Eastern
    study_desc %in% c('Arab',
                      'Arabic',
                      'Emirati',
                      'Greater Middle Eastern (Middle Eastern, North African Or Persian)',
                      'Greater Middle Eastern',
                      'North African',
                      'Arabic/North African',
                      'Saudi Arab',
                      'Saudi Arabian',
                      'Lebanese',
                      'Persian',
                      'Middle Eastern',
                      'Qatari') ~
      'Greater Middle Eastern (Middle Eastern, North African or Persian)',

    # Sub-Saharan African
    study_desc %in% c('South African',
                      'West African',
                      'Sub-Saharan African',
                      'Kenyan',
                      'Ethiopian',
                      'Zulu') ~ "Sub-Saharan African",

    # South Asian
    study_desc %in% c('Indian',
                      'Indian Asian',
                      'Asian Indian',
                      'Indo-European',
                      'Northern Indian',
                      'Fijian Indian',
                      'Pakistani',
                      'Bangladeshi',
                      'Punjabi Sikh',
                      #'Bangladeshis, Pakistanis',
                      'South Asian',
                      'Sindhi') ~ "South Asian",

    # South East Asian
    study_desc %in% c('South East Asian',
                      'Malaysian',
                      'Malay',
                      'Thai',
                      'Vietnam',
                      'Vietnamese') ~ "South East Asian",


    # Hispanic or Latin American
    study_desc %in% c('Brazilian',
                      'Costa Rican',
                      'Chilean',
                      'Hispanic',
                      'Hispanic/Latin American',
                      'Hispanic Or Latino',
                      'Latin American/Hispanic',
                      'Hispanic/Latino',
                      'Hispanic / Latino American',
                      'Hispanic American',
                      'Mexican And Other Latin American',
                      'Mexican',
                      'Mexican American',
                      'Hispanic Or Latin American',
                      'Indigenous Mexican',
                      'Admixed American',
                      'Latino',
                      'Latin American') ~ 'Hispanic or Latin American',

    # Native American
    study_desc %in% c("Native American",
                      "Alaskan Native",
                      'American Indian',
                      'American Indian/Alaskan Native',
                      'Pima Indian',
                      'Maya'
    ) ~ "Native American",

    # Not reported
    study_desc %in% c("NR",
                      'Nr',
                      "Unknown",
                      "Other Ancestries") ~ "NR",

    # Oceanian
    study_desc %in% c("Native Hawaiian",
                      'Pacific Islander',
                      'Oceanian'
    ) ~ "Oceanian",

    # Aboriginal Australian
    study_desc %in% c(
                      "Martu Australian Aboriginal",
                      'Aboriginal Australian'
    ) ~ "Aboriginal Australian",

    # Other
    study_desc %in% c('Other') ~ "Other",

    # Other Admixed
    study_desc %in% c('Other-Admixed',
                      'Mixed And Other',
                      'Mixed Race',
                      'Other Admixed',
                      'Greenlandic Inuit',
                      'Greenlandic') ~
      "Other admixed ancestry",

    TRUE ~ paste0("UNMATCHED: ", study_desc)
  )

  return(pop_lab)
}


get_sex_from_sample_desc <- function(sample_description){

  sex_terms <-
    c("\\bmen\\b",
      "\\bwomen\\b",
      "\\bmale(?=s|\\s)",
      "\\bfemale(?=s|\\s)",
      "\\bboy(?=s|\\s)",
      "\\bgirl(?=s|\\s)"
    )
  pattern <- paste0(sex_terms,
                    collapse = "|")

  matches <- str_extract_all(tolower(sample_description),
                             pattern)

  sex_desc <-purrr::map_chr(matches,
                 \(x) paste0(x, collapse = ";"))

  sex_status <-
  case_when(sex_desc %in% c("boy",
                            "male",
                             "men") ~ "male",
            sex_desc %in% c("girl",
                            "female",
                            "women") ~ "female",
            TRUE ~ "NR")

  return(sex_status)
}


get_case_status_from_sample_desc = function(sample_description){

  case_terms <-
    c("\\bcase(?=(s|\\s|$))",
      "\\bcontrol(?=(s|\\s|$))",
      "\\bhealthy\\s",
      "(?<=(-|\\s))affected(?=(\\s|$))",
      "(?<=(-|\\s))unaffected(?=(\\s|$))",
      "(?<!case )(?<!cases )with (?=(cervical cancer|myocardial infarction)(?! (?:data|information|measurement)))"
    )
  pattern <- paste0(case_terms,
                    collapse = "|")

  matches <- str_extract_all(tolower(sample_description),
                             pattern)

  case_desc <-purrr::map_chr(matches,
                            \(x) paste0(sort(unique(x)),
                                        collapse = "; "))


  case_status <-
    case_when(case_desc %in% c("case",
                              "affected",
                              "with ") ~ "case",
              case_desc %in% c("control",
                               "unaffected",
                               "healty") ~ "control",
              case_desc %in% c("case; control")  ~ "all",
              TRUE ~ "NS")

  return(case_status)

}
