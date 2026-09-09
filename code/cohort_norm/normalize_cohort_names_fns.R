title_except_caps <- function(x) {
  vapply(
    stringr::str_split(x, " "),
    function(words) {
      paste(
        ifelse(
          stringr::str_detect(words, "[a-z]"),
          stringr::str_to_title(words),
          words
        ),
        collapse = " "
      )
    },
    character(1)
  )
}


# create paper specific abbreviation -> long form pairs
extract_abbrev_pairs_one <- function(pubmed_id,
                                     sentence,
                                     cohorts
) {

  empty <- data.frame(abbrev = NA_character_,
                      long = NA_character_,
                      sentence = sentence,
                      pubmed_id = pubmed_id
  )

  # if sentence is empty, skip
  if (length(sentence) != 1L || is.na(sentence) || !nzchar(sentence)) {

    return(empty)

  }

  parentheses_loc <- stringr::str_locate_all(pattern = "\\(([^()]{2,80})\\)",
                                             string = sentence) |>
    purrr::map(.f = as.data.frame) |>
    purrr::list_rbind()

  cohorts <- stringr::str_split(pattern = "; ",
                                cohorts) |>
    unlist() |>
    stringr::str_trim()

  # if can't find parentheses, skip
  if(nrow(parentheses_loc) == 0){

    return(empty)

  }

  starts <- parentheses_loc$start
  ends <- parentheses_loc$end

  long <- character()
  abbrev <- character()

  for (bracket_n in seq_along(starts)) {

    # extract inside bracket
    inner  <- stringr::str_sub(
      sentence,
      starts[bracket_n] + 1,  # start excluding brackets
      ends[bracket_n] - 1)  |>
      stringr::str_trim()

    # check is this in cohort?
    if(!inner %in% cohorts){
      next
    }

    # get text before bracket
    before <- stringr::str_sub(
      sentence,
      1,
      starts[bracket_n] - 1
    )

    # possible cohort match, before:
    cohort_match <-
      stringr::str_locate_all(pattern = stringr::fixed(cohorts[cohorts != inner]),
                              before)   |>
      purrr::map(.f = as.data.frame) |>
      purrr::list_rbind()

    # keep the closest match
    if (all(is.na(cohort_match$start))) {
      next
    }

    cohort_match <- cohort_match |>
      dplyr::slice_max(end,
                       with_ties = F)


    # count number of words between
    words_between <-
      stringr::str_sub(
        sentence,
        cohort_match$end + 1,
        starts[bracket_n]
      ) |>
      stringr::str_remove_all(pattern = "\\(") |>
      stringr::str_trim() |>
      stringr::str_count(pattern = "\\s+")



    # skip this match if number of words between is more than 2
    if(words_between > 2){
      next
    }

    cohort_match <- stringr::str_sub(
      sentence,
      cohort_match$start,
      cohort_match$end
    )

    cohort_names <- c(inner, cohort_match)
    # order in terms of string length
    cohort_names <- cohort_names[rank(stringr::str_length(cohort_names)
    )]

    abbrev[bracket_n] <- cohort_names[1]
    long[bracket_n] <- cohort_names[2]

  }

  if(all(is.na(abbrev))){
    return(empty)
  }

  abbrev_df <- data.frame(abbrev = abbrev,
                          long = long,
                          sentence = sentence,
                          pubmed_id = pubmed_id) |>
    distinct() |>
    filter(!is.na(abbrev))

  return(abbrev_df)

}


clean_cohort_names <- function(cohort_names){

  cleaned_names <-
    stringi::stri_trans_general(cohort_names,
                                "Latin-ASCII")

  # remove commas
  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                           ",")

  # remove apostrophes
  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                           "’s|'s|'|’")

  # remove underscores
  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                           "_")

  # remove colon at end of cohort
  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                           ":$")

  # remove hyphens
  cleaned_names <- stringr::str_replace_all(pattern = "-|–|‐",
                                            replacement = " ",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "-|–|‐",
                                            replacement = " ",
                                            cleaned_names)

  # replace and with &
  cleaned_names <- stringr::str_replace_all(
    cleaned_names,
    "(?<=\\b|23)[Aa]nd(?=\\b|[Mm])",
    "&"
  )

  return(cleaned_names)

}

  # split UKBioBank into UK BioBank
  cleaned_names <- stringr::str_replace_all(pattern = stringr::regex("(?<=\\S)(?=biobank)",
                                                                     ignore_case = TRUE),
                                            replacement = " ",
                                            cleaned_names)


  # replace UK with United Kingdom
  cleaned_names <- stringr::str_replace_all(pattern = "\\bUK\\b|\\bU\\.K\\.\\b",
                                            replacement = "United Kingdom",
                                            cleaned_names)

  # replace UK with United Kingdom
  cleaned_names <- stringr::str_replace_all(pattern = "\\bUS\\b",
                                            replacement = "United States",
                                            cleaned_names)

  # replace COPD with
  cleaned_names <- stringr::str_replace_all(pattern = "\\bCOPD\\b",
                                            replacement = "Chronic Obstructive Pulmonary Disease",
                                            cleaned_names)

  # replace MI with
  cleaned_names <- stringr::str_replace_all(pattern = "\\bMI\\b",
                                            replacement = "Myocardial Infarction",
                                            cleaned_names)

  # replace AD with
  cleaned_names <- stringr::str_replace_all(pattern = "\\bAD\\b",
                                            replacement = "Alzheimer Disease",
                                            cleaned_names)

  # M.D. -> MD
  cleaned_names <- stringr::str_replace_all(pattern = "\\bM\\.D\\.\\b",
                                            replacement = "MD",
                                            cleaned_names)





  # replace V before a number with Version
  cleaned_names <- stringr::str_replace_all(
    cleaned_names,
    "\\b[Vv](?=\\d+)",
    "Version "
  )

  # add space between letters and roman numerals
  cleaned_names <- stringr::str_replace_all(
    cleaned_names,
    "(?<=[a-z])(?<![IVXLCDM])(?=I{1,3}\\b)",
    " "
  )

  # add space between letters and numbers
  cleaned_names <- stringr::str_replace_all(
    cleaned_names,
    "(?<=[A-Za-z])(?=[1-5]\\b)",
    " "
  )

  # remove citation weirdness
  cleaned_names <- ifelse(
    stringr::str_detect(cleaned_names, "\\s") & stringr::str_detect(cleaned_names, "[a-z]{2,}\\d{2,3}$"),
    stringr::str_remove_all(pattern = "\\d+$", cleaned_names),
    cleaned_names
  )

  # convert all numbers to arabic numerals
  cleaned_names <- stringr::str_replace_all(pattern = "\\bIV\\b",
                                            replacement = "4",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bIII\\b",
                                            replacement = "3",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bII\\b",
                                            replacement = "2",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bI\\b",
                                            replacement = "1",
                                            cleaned_names)

  cleaned_names  <- stringr::str_squish(cleaned_names)

  # if multi-word cohort, title case:
  cleaned_names <- ifelse(
    stringr::str_detect(cleaned_names, "\\s"),
    title_except_caps(cleaned_names),
    cleaned_names
  )

  # if single word, and all lower case, make upper case
  cleaned_names <- ifelse(
    !stringr::str_detect(cleaned_names, "\\s") & tolower(cleaned_names) == cleaned_names,
    stringr::str_to_upper(cleaned_names),
    cleaned_names
  )

  cleaned_names <- stringr::str_remove_all(pattern = " &$|^& ",
                                           cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bStudies\\b",
                                            replacement = "Study",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bMetaanalysis\\b",
                                            replacement = "Meta Analysis",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bGenetic$",
                                            replacement = "Genetics",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bGenetic$",
                                            replacement = "Genetics",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bGenebank\\b",
                                            replacement = "Gene Bank",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "\\bEnd Points\\b",
                                            replacement = "Endpoints",
                                            cleaned_names)


  return(cleaned_names)

}


remove_project_terms <- function(cohort_names){


  cleaned_names <- stringr::str_remove_all(cohort_names,
                                           "\\s+Cohort$|\\s+Study$|\\s+Project$|\\s+Consortium$|\\s+Consortia")

  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                           "\\s+Research Program$|\\s+Program|\\s+Sample$|\\s+Biorepository$|\\s+Collaboration$")


  cleaned_names <- stringr::str_squish(cleaned_names)

  # drop cohort / non-specific terms
  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                       "^Cohort$|^COHORT$|^Consortium$|^CONSORTIUM$|^Freeze$|^FREEZE$")

  cleaned_names <- stringr::str_remove_all(cleaned_names,
                                           "^BIOBANK$|Biobank$")


  cleaned_names <- stringr::str_squish(cleaned_names)

  return(cleaned_names)

}



consistent_transcriptions <- function(cohort_names){

  cleaned_names <- stringr::str_replace_all(pattern = "COVID(?! 19|19)",
                                            replacement = "COVID 19",
                                            cohort_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Haemorrhagic",
                                            replacement = "Hemorrhagic",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Ischaemic",
                                            replacement = "Ischemic",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Analyses",
                                            replacement = "Analysis",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Anthropmetric",
                                            replacement = "Anthropometric",
                                            cleaned_names)


  return(cleaned_names)

}

consistent_plurals <- function(cohort_names){

  cleaned_names <- stringr::str_replace_all(pattern = "Trials",
                                            replacement = "Trial",
                                            cohort_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Genetics",
                                            replacement = "Genetic",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Veteran\\b",
                                            replacement = "Veterans",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Lipid\\b",
                                            replacement = "Lipids",
                                            cleaned_names)

  cleaned_names <- stringr::str_replace_all(pattern = "Religious Order\\b",
                                            replacement = "Religious Orders",
                                            cleaned_names)


  return(cleaned_names)

}


