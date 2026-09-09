# functions for checking that ICD-10 Codes are valid WHO 2019 Codes

# read in the WHO ICD-10 2019 code list and metadata
read_icd10_meta <- function(path = here::here("data/icd/icd102019enMeta/icd102019syst_codes.txt")) {

  cols <- c("level", "node", "terminal_type", "chapter", "block",
            "code_raw", "code_dot", "code",
            "title", "title3", "title4", "title5",
            "morb", "mort4", "mort3", "mort2", "mort1")

  d <- read.table(path,
                  sep = ";",
                  header = FALSE,
                  quote = "",
                  comment.char = "",
                  encoding = "UTF-8",
                  colClasses = "character",
                  col.names = cols, strip.white = TRUE)

  d[d == "UNDEF"] <- NA
  d$terminal <- d$node == "T"
  d$asterisk <- grepl("*",
                      d$code_raw,
                      fixed = TRUE)
  d
}

# check codes are valid ICD-10 codes, and return a data frame with the input code, matched code, title, and validation status
validate_icd10 <- function(codes, # vector of ICD-10 Codes
                           tab, # data frame of valid ICD-10 Codes and metadata (from read_icd10_meta)
                           allow_nonterminal = TRUE) {

  # ensure read in codes are in the correct format for matching
  normalize_icd10 <- function(x) {

    gsub("[^A-Z0-9]", "", toupper(trimws(x)))
  }


  i <- match(normalize_icd10(codes), tab$code)

  status <- case_when(
    is.na(i)                              ~ "unknown",
    !allow_nonterminal & !tab$terminal[i] ~ "code to a further character",
    tab$asterisk[i]                       ~ "ok (asterisk / secondary only)",
    TRUE                                  ~ "ok"
  )

  data.frame(
    input     = codes,
    matched   = tab$code_dot[i],
    title     = tab$title[i],
    level     = tab$level[i],       # 3 / 4 / 5
    terminal  = tab$terminal[i],    # FALSE for rollups like C34
    status    = status,
    valid     = !is.na(i) & (allow_nonterminal | tab$terminal[i] %in% TRUE),
    row.names = NULL
  )
}
