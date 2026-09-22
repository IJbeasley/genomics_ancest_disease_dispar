ascii_key <- function(x) {
  x |>
    stringi::stri_enc_toutf8(validate = TRUE) |>   # malformed bytes -> U+FFFD
    stringi::stri_trans_nfc() |>                   # é as e+U+0301 folds like é as U+00E9
    stringi::stri_trans_general("Latin-ASCII") |>  # é->e  ö->o  ç->c  ß->ss
    stringi::stri_replace_all_regex("[\u2010-\u2015]", "-") |>  # en/em dashes
    stringi::stri_replace_all_regex("[^\\p{ASCII}]", "") |>     # U+FFFD, β, × ...
    stringi::stri_trim_both()
}


clean_trait_documentation <- function(trait_vector) {

  trait_clean <- trait_vector |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_remove_all('["\u201C\u201D]')

  trait_clean <- trait_clean |>
    str_remove_all("(?i)\\s*\\(UKB data field[^)]*\\)") |>
    str_remove_all("(?i)\\s*\\(\\d+_\\d+\\)\\s*$") |>
    str_remove_all("(?i)\\s*\\(Phecode \\d+(?:\\.\\d+)?\\)") |>
    str_remove_all("(?i)\\s*RELEASE\\s*\\d+\\s*$") |>
    str_remove_all("(?i)\\s*\\((?:agilent|twist)_\\d+\\)") |>
    str_remove_all("(?i)(?<=Source of report of\\s)[A-Z][0-9]{1,2}(?:\\.[0-9A-Z]+)?\\b") |>
    str_remove_all("(?i)(?<=ICD-10\\s)[A-Z][0-9]{1,2}(?:\\.[0-9A-Z]+)?\\b") |>
    str_remove_all("(?i)(?<=ICD10 )[^:]+(?=:)|(?<=ICD10 )[^ ]+") |>
    str_remove_all("(?i)ICD10")


  # --- remove model related terms ---
  meta_paren <- paste0(
    "(?i)\\s*\\((?:",
    paste(
      c(
        "age (of|at) (onset|diagnosis)",
        "age of onset \\< 21",
        "\\d+\\s*df(?:\\s+test)?",
        "(?:SPA|Firth)\\s+correction",
        "gene(?:[- ]based )?burden",
        "gene burden",
        "does not include regular naps",
        "time to event",
        "survival",
        "\\d+-day mortality",
        "model\\s*\\d+",
        "ICD-10 coded",
        "change over time",
        "apnea hypopnea index, change over time",
        "joint analysis[^)]*",
        "trans-disease meta-analysis",
        "multivariate analysis",
        "multi-trait analysis",
        "MTAG",
        "pleiotropy",
        "CNV (?:mirror|U-shape) model",
        "case-control set\\s*\\d+",
        "adjusted for[^)]*",
        "[^)]*\\badjusted\\b[^)]*",
        "percentage of [^)]*",
        "[^)]*\\bper event\\b[^)]*",
        "[^)]*\\bsleep episode\\b[^)]*",
        "[^)]*\\bChecklist[^)]*",
        "concentration drug ratio",
        "severest",
        "time to remission",
        "population-based",
        "baseline",
        "pairwise",
        "\\d+\\s*(?:month|week|year)\\s*visit",
        "self[ -]report(?:ed)?",
        "RELEASE\\s*\\d+",
        "ordinary least squares \\(OLS\\)",
        "(?:SPACox|ADuLT) model[^)]*",
        "(?:standard|weighted)\\s+GWA",
        "(?:binary|ordinal) trait",
        "[Cc]onditioned on rs\\d+",
        "conditioned\\s+on\\s+rs\\s*\\d+",
        "AlphaMissense",
        "ESRD vs. no ESRD",
        "treatment strategy interaction"
      ),
      collapse = "|"
    ),
    ")\\)"
  )


  trait_clean <- trait_clean |>
    str_remove_all(meta_paren) |>
    str_remove_all("(?i)\\s*\\([^)]*\\binteraction\\b[^)]*\\)") |>
    str_remove_all("(?i)\\s*\\([^)]*\\bvs\\.?\\b[^)]*\\)") |>
    str_remove_all("(?i) adjusted for[^)]*$") |>
    str_remove_all("(?i)(,|:) PHESANT recoding") |>
    stringr::str_remove_all(pattern = "(?i)\\((.*?) genetic model\\)")


  # ---- remove clinical information ---- #
  trait_clean <- trait_clean |>
    str_remove_all("(?i)\\s*\\(hospital (?:diagnosed|admission)\\)") |>
    str_remove_all("(?i)\\([^)]* classification\\)") |>
    str_remove_all("(?i)\\s*\\(\\w+(?: \\w+)? scale\\)") |>
    str_remove_all("(?i)\\s*\\(\\w+(?: \\w+)? score\\)") |>
    str_remove_all("(?i)\\s*\\(\\w+(?: \\w+)? index\\)") |>
    str_remove_all("(?i)\\s*\\(\\w+(?: \\w+)? factor\\)") |>
    str_remove_all("(?i)\\s*\\(clinical subgroup [^)]*\\)") |>
    str_remove("(?i)^(?:cancer|non-cancer illness|treatment/medication|operation) code,\\s*self-reported:\\s*") |>
    str_remove_all("(?i)Takes medication for ") |>
    str_remove_all("(?i)Viral variant \\w+ carrier status in\\s*") |>
    str_remove_all("(?i)3-month functional outcome in\\s*") |>
    str_remove_all("(?i)\\s*\\((XELOX|FOLFOX)(?:,\\s*\\w+)?\\)") |>
    str_remove_all("(?i)\\s*\\(\\d+[- ]weeks?\\)") |>
    str_remove_all("(?i)(within|but not) the last 12 months") |>
    str_remove_all("(?i)\\(advanced\\)") |>
    str_remove_all("(?i)\\(prostate cancer excluded\\)")


  trait_clean <- trait_clean |>
    stringr::str_replace(
      pattern = "(?i)^age at (.*?) onset",
      replacement = "\\1"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\bage at diagnosis|x age interaction\\b|\\bx sex\\b"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\(age at diagnosis\\)|\\(age-stratified\\)"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)age of onset of"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)^age at\\b"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)- year age first occurred"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)^interpolated age of participant when"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)epigenetic age acceleration"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)at diagnosis|first diagnosed|first began"
    )


  trait_clean <- trait_clean |>
    str_remove_all("(?i)\\([^)]* mutations?\\)") |>
    str_remove_all("(?i)\\([^)]* mutation only\\)") |>
    str_remove_all("(?i)\\s*\\(\\w+ deletions?\\)") |>
    str_remove_all("(?i)\\s*\\(rs\\d+\\)") |>
    str_remove_all("(?i)\\s*\\(rs\\d+_\\d+\\)")


  trait_clean <- trait_clean |>
    stringr::str_replace(
      pattern = "(?i)^time to (.*?) recurrence",
      replacement = "\\1"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)^time to|^survival time"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\(survival time\\)|\\(time to progression\\)"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)specific survival"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\(time to first abdominal surgery\\)|\\(time to first treatment\\)"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\(time to productions\\)|\\(time to recurrence up to 6 months\\)"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\(proximal\\)|\\(distal\\)|\\(left-sided\\)|\\(right-sided\\)"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)\\(1 week after surgery\\)|\\(3 months after surgery\\)"
    )



  trait_clean <- trait_clean |>
    str_remove_all(
      pattern = "(?i): (?:yes|no)"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)x occupational exposure"
    ) |>
    str_remove_all(
      pattern = "(?i)\\(biological dust, mineral dust, gases and fumes, pesticides, aromatic solvents, chlorinated solvents, other solvents or metals\\) interaction"
    )




  trait_clean <- trait_clean |>
    stringr::str_remove_all(
      pattern = "(?i)^progression free survival in|^survival in|^disease free survival in"
    ) |>
    stringr::str_replace(
      pattern = "(?i)^15-year (.*?) survival",
      replacement = "\\1"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)^recurrence of"
    )


  trait_clean <- trait_clean |>
    stringr::str_remove_all(
      pattern = "(?i)- more than half the days|- nearly every day|- several days"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)- quite a bit|- moderately| - a little bit|- not at all"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)in last 2 weeks|in past month"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)hq-9 questionnaire-|^ever had"
    ) |>
    stringr::str_remove_all(
      pattern = "(?i)in monozygotic twins"
    )


  trait_clean <- trait_clean |>
    str_remove_all(
      "(?i)\\w+ expression at (baseline|\\d+ weeks) in (whole blood|PBMC)"
    ) |>
    str_remove_all(
      "(?i)upon \\w+ stimulation in\\s+"
    ) |>
    str_remove_all(
      "(?i)^baseline \\w+ levels in response to \\w+ \\w+ \\w+ \\w+ test in\\s+"
    )


  trait_clean <- trait_clean |>
    str_remove_all(
      paste0(
        "(?i)\\s*\\((?:maternal|paternal|inherited|fetal|child|recipient|donor|",
        "parent[- ]of[- ]origin|maternal genetic|",
        "(?:maternal|paternal|offspring|fetal) genotype|",
        "maternal and offspring genotype)? ?effects?\\)"
      )
    ) |>
    str_remove_all("(?i)(family|paternal) history of") |>
    str_remove_all("(?i)Biological Grandparent \\((?:maternal|paternal)\\):\\s*") |>
    str_remove_all("(?i)Biological Grandparent \\(maternal\\) \\(maternal\\):") |>
    str_remove_all("(?i)Biological (?:Father|Mother|Sibling):\\s*") |>
    str_remove_all("(?i)Illnesses of (?:father|mother|siblings)[: -]\\s*") |>
    str_remove_all("(?i)\\((maternal|paternal)\\)") |>
    str_remove_all("(?i)\\s+families$") |>
    str_remove_all("(?i)Maternal genotype effects in")


  trait_clean <- trait_clean |>
    str_remove_all("(?i)Behaviour of cancer tumour (-|:)")


  trait_clean <- trait_clean |>
    str_remove_all("\\?") |>
    str_replace_all("\\s*/\\s*", "/") |>
    str_replace_all("\\s{2,}", " ") |>
    str_squish() |>
    str_remove("^[\\s,;:.-]+") |>
    str_remove("[\\s,;:.-]+$") |>
    stringr::str_remove_all("'|’") |>
    stringr::str_replace_all(pattern = ",$",
                             replacement = "") |>
    ascii_key()

  trait_clean <- tolower(trait_clean)

  return(trait_clean)
}

