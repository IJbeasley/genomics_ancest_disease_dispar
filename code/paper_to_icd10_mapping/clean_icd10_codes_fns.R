
normalize_icd10_codes <- function(icd10_vector) {
  # every dash-like character
  ICD10_DASH_RE <- "[‐‑‒–—―−﹘﹣－]"

  normalized_icd10 <-
    icd10_vector |>
    # make dash like character consistent
    str_replace_all(ICD10_DASH_RE, "-") |>
    str_to_upper() |>
    # separators other than the comma
    str_replace_all("[;|/]", ",") |>
    # a space doing the job of a comma ("E11 E12"), but not the space after one
    str_replace_all("(?<=[0-9A-Z])[[:space:]]+(?=[A-Z][0-9])", ", ") |>
    # a dot left dangling before a comma or at the end ("E11.," / "E11.")
    str_remove_all("\\.(?=[[:space:]]*(,|$))") |>
    # consistent spacing around commas and none around dashes
    str_replace_all("[[:space:]]*,[[:space:]]*", ", ") |>
    str_replace_all("[[:space:]]*-[[:space:]]*", "-") |>
    # leading / trailing punctuation
    str_remove_all("^[[:space:],]+|[[:space:],]+$") |>
    # removing decimals where there should only be commas
    str_replace_all(pattern = "\\.,",
                    replacement = ",") |>
    str_squish()

  return(normalized_icd10)
}


# must be applied rowwise
simplify_range <- function(icd10_range) {
  # two of those joined by a hyphen
  ICD10_RANGE_RE <- paste0(
    "^([A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?)",
    "-",
    "([A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?)$"
  )

  icd10_range_split <-
    stringr::str_split(icd10_range, "-") |>
    unlist()

  stopifnot(length(icd10_range_split) == 2)

  # if end of range is 0.9, remove
  icd10_range_split [2] <- str_replace(icd10_range_split [2], "\\.9$", "")

  icd10_range <- str_flatten(unique(icd10_range_split),
                             collapse = "-")

  return(icd10_range)

}


# must be applied rowwise
expand_range <- function(icd10_range){

  # two of those joined by a hyphen
  ICD10_RANGE_RE <- paste0(
    "^([A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?)",
    "-",
    "([A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?)$"
  )

  stopifnot(grepl(ICD10_RANGE_RE, icd10_range))

  icd10_range_split <-
    stringr::str_split(icd10_range, "-") |>
    unlist()

  if(any(grepl("\\.", icd10_range_split))){

    # if all codes in the range have the same first 3 characters, expand to include all codes in the range
    if(length(unique(stringr::str_remove_all(icd10_range_split, "\\.\\d+"))) == 1){

      start_code <- icd10_range_split[1]
      end_code <- icd10_range_split[2]

      start_code_no_decimal <- stringr::str_remove(start_code, "\\.\\d+")
      end_code_no_decimal <- stringr::str_remove(end_code, "\\.\\d+")

      stopifnot(start_code_no_decimal == end_code_no_decimal)

      start_num <- as.numeric(stringr::str_extract(start_code, "(?<=\\.)\\d+"))
      end_num <- as.numeric(stringr::str_extract(end_code, "(?<=\\.)\\d+"))

      if(is.na(end_num)){

        end_num <- 9

      }

      if(is.na(start_num)){

        start_num <- 0

      }

      stopifnot(start_num <= end_num)

      expanded_codes <- paste0(start_code_no_decimal, ".",
                               sprintf("%d", seq(start_num, end_num)))

    }

    else {
      expanded_codes <- icd10_range

    }

  } else {

    # if just codes without decimals, expand to include all codes in the range
    start_code <- icd10_range_split[1]
    end_code <- icd10_range_split[2]

    start_letter <- stringr::str_extract(icd10_range_split[1], "[A-Z]")
    end_letter <- stringr::str_extract(icd10_range_split[2], "[A-Z]")

    if(start_letter == end_letter){

      start_num <- as.numeric(stringr::str_extract(icd10_range_split[1], "\\d+"))
      end_num <- as.numeric(stringr::str_extract(icd10_range_split[2], "\\d+"))

      stopifnot(start_num <= end_num)

      expanded_codes <- paste0(start_letter, sprintf("%02d", seq(start_num, end_num)))

    } else {
      # if letters are different
      stopifnot(which(LETTERS == start_letter) < which(LETTERS == end_letter))

      letter_range <- LETTERS[which(LETTERS == start_letter):which(LETTERS == end_letter)]

      expanded_codes  <- c()

      for(letter_code in letter_range){

        if(letter_code == start_letter){

          start_num <- as.numeric(stringr::str_extract(icd10_range_split[1], "\\d+"))
          end_num <- 99

          expanded_codes <-
            append(expanded_codes,
                   paste0(letter_code,
                          sprintf("%02d",
                                  seq(start_num, end_num))
                   )
            )


        } else if(letter_code != end_letter & letter_code != start_letter){

          start_num <- 0
          end_num <- 99

          expanded_codes <-
            append(expanded_codes,
                   paste0(letter_code,
                          sprintf("%02d",
                                  seq(start_num, end_num))
                   )
            )


        } else if(letter_code == end_letter){

          start_num <- 0
          end_num <- as.numeric(stringr::str_extract(icd10_range_split[2], "\\d+"))

          expanded_codes <-
            append(expanded_codes,
                   paste0(letter_code,
                          sprintf("%02d",
                                  seq(start_num, end_num))
                   )
            )

        }

      }



    }

  }

  # excluding any codes that are not valid ICD10 codes
  not_valid_codes <- c(# A9-14
    sprintf("A%02d",
            seq(10, 14)),
    "A29",
    "A45",
    "A47",
    "A61",
    "A62",
    "A72",
    "A73",
    "A76",
    "A90",
    "A91",
    sprintf("B%02d",
            seq(10, 14)),
    "B28",
    "B29",
    "B31",
    "B32",
    "B59",
    sprintf("B%02d",
            seq(61, 64)),
    "B84",
    "B93",
    sprintf("C%02d",
            seq(27, 29)),
    "C35",
    "C36",
    "C42",
    "C59",
    "C87",
    "C89",
    "D29.5",
    "D29.6",
    "D29.8",
    sprintf("E%02d",
            seq(8, 9)),
    "E81",
    "E82",
    sprintf("F%02d",
            seq(35, 37)),
    sprintf("G%02d",
            seq(15, 19)),
    sprintf("G%02d",
            seq(27, 29)),
    sprintf("G%02d",
            seq(33, 34)),
    sprintf("G%02d",
            seq(38, 39)),
    "G42",
    sprintf("G%02d",
            seq(48, 49)),
    sprintf("G%02d",
            seq(65, 69)),
    sprintf("G%02d",
            seq(74, 79)),
    sprintf("G%02d",
            seq(84, 89)),
    sprintf("I%02d",
            seq(3, 4)),
    "I14",
    sprintf("I%02d",
            seq(16, 19)),
    sprintf("I%02d",
            seq(53, 59)),
    "I75",
    "I76",
    sprintf("I%02d",
            seq(90, 94)),
    "I96",
    "J07",
    "J08",
    "J19",
    sprintf("J%02d",
            seq(23, 29)),
    sprintf("J%02d",
            seq(48, 59)),
    sprintf("J%02d",
            seq(71, 79)),
    "J83",
    sprintf("J%02d",
            seq(87, 89)),
    "J97",
    sprintf("K%02d",
            seq(15, 19)),
    "K24",
    sprintf("K%02d",
            seq(32, 34)),
    "K39",
    sprintf("K%02d",
            seq(47, 49)),
    "K53",
    "K54",
    "K68",
    "K69",
    "K78",
    "K79",
    "K84",
    "K88",
    "K89",
    "L06",
    "L07",
    "M04",
    "M12.9",
    "M74",
    "M64",
    "M69",
    "M78",
    "N65",
    "N78",
    "N79"
  )


  expanded_codes <- expanded_codes[!c(expanded_codes %in% not_valid_codes)]

  expanded_codes <- stringr::str_flatten_comma(unique(expanded_codes),
                                               na.rm = T)


  return(expanded_codes)
}
