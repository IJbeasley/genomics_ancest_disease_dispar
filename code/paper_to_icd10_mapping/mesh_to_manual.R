# ==============================================================================
# ICD-10 range algebra + GBD 2019 cause mapping
# ------------------------------------------------------------------------------
# Codes are mapped onto a single monotone numeric axis:
#
#     value = (letter_index * 36^2) + d2 * 36 + v36(d3) + fraction(subchars)
#
# Every code covers a half-open block [lo, hi]:
#     lo = value
#     hi = value + 36^(-n_subchars) - EPS      (n = 0  ->  width 1)
#
# so "K75" spans all of K75.xx, "K75.4" spans all of K75.4x, etc.
# Because the leading letter is baked into the value, ranges that cross a letter
# boundary ("A00-B99") work without special-casing.
# ==============================================================================

{
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(readxl)
library(here)
}

# ---- constants ---------------------------------------------------------------

B36    <- c(as.character(0:9), LETTERS)  # base-36 alphabet
STRIDE <- 36^2                           # numeric span reserved per leading letter
EPS    <- 1e-9                           # half-open-interval fudge factor
SRC    <- c("overlap", "icd10_non_fatal_only", "icd10_cod_only")  # match priority

v36 <- function(ch) {
  i <- match(toupper(ch), B36)
  if (anyNA(i)) stop("Invalid base-36 character(s): ",
                     paste(ch[is.na(i)], collapse = ", "))
  i - 1L
}

# An interval table always has these columns, even when empty, so that
# bind_rows()/unnest() never silently drop the schema.
empty_iv <- function() {
  tibble(sv = numeric(), ev = numeric(),
         sl = character(), el = character(),
         sl_open = logical(), el_open = logical())
}

# Pretty-print intervals. ">" / "<" mark an *exclusive* endpoint, which is what
# subtraction produces (e.g. ">A09-A18" = everything after A09 up to A18).
fmt_iv <- function(sl, el, sl_open, el_open) {
  paste0(if_else(sl_open, ">", ""), sl,
         if_else(sl == el & !sl_open & !el_open,
                 "",
                 paste0("-", if_else(el_open, "<", ""), el)))
}

show_ranges <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  paste(fmt_iv(df$sl, df$el, df$sl_open, df$el_open), collapse = ", ")
}

# ---- parsing -----------------------------------------------------------------

parse_icd10_code <- function(code) {
  raw  <- str_trim(code)
  norm <- raw |>
    str_remove_all("[\u2020*+\\s]") |>     # dagger / asterisk / stray whitespace
    str_remove("[.\\-\u2013\u2014]+$")     # trailing placeholder, e.g. "A09.-"

  m <- str_match(norm, "^([A-Za-z])([0-9])([0-9A-Za-z])\\.?([0-9A-Za-z]*)$")
  if (is.na(m[1, 1])) stop("Unparseable ICD-10 code: '", raw, "'")

  sub  <- m[1, 5]
  frac <- 0
  if (nchar(sub) > 0) {
    chars <- str_split(sub, "")[[1]]
    frac  <- sum(v36(chars) / 36^seq_along(chars))
  }

  letter <- toupper(m[1, 2])
  value  <- (match(letter, LETTERS) - 1L) * STRIDE +
    v36(m[1, 3]) * 36 + v36(m[1, 4]) + frac
  width  <- if (nchar(sub) == 0) 1 else 36^(-nchar(sub))

  list(letter = letter,
       lo     = value,
       hi     = value + width - EPS,   # a truncated code covers its whole block
       label  = raw)
}

parse_range <- function(rng) {
  parts <- str_split(rng, "[-\u2013\u2014]")[[1]] |> str_trim()
  parts <- parts[parts != ""]
  if (length(parts) == 0) stop("Empty ICD-10 range: '", rng, "'")

  s <- parse_icd10_code(parts[1])
  e <- parse_icd10_code(parts[length(parts)])

  if (s$lo > e$hi) {                       # e.g. a "K75.4-K75" typo in the source
    warning("Reversed ICD-10 range dropped: '", rng, "'", call. = FALSE)
    return(empty_iv())
  }
  tibble(sv = s$lo, ev = e$hi, sl = s$label, el = e$label,
         sl_open = FALSE, el_open = FALSE)
}

parse_rangelist <- function(s) {
  if (length(s) != 1 || is.na(s) || str_trim(s) == "") return(empty_iv())
  parts <- str_split(s, "[,;]")[[1]] |> str_trim()
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(empty_iv())
  bind_rows(lapply(parts, parse_range))
}

# ---- interval algebra --------------------------------------------------------

merge_ranges <- function(df) {
  if (nrow(df) == 0) return(empty_iv())
  x   <- arrange(df, sv, ev)
  cur <- x[1, ]
  out <- list()
  for (k in seq_len(nrow(x))[-1]) {
    if (x$sv[k] <= cur$ev + EPS) {                 # overlapping or touching
      if (x$ev[k] > cur$ev) {
        cur$ev      <- x$ev[k]
        cur$el      <- x$el[k]
        cur$el_open <- x$el_open[k]
      }
    } else {
      out[[length(out) + 1]] <- cur
      cur <- x[k, ]
    }
  }
  out[[length(out) + 1]] <- cur
  bind_rows(out)
}

intersect_ranges <- function(a, b) {
  if (nrow(a) == 0 || nrow(b) == 0) return(empty_iv())
  out <- list()
  for (i in seq_len(nrow(a))) for (j in seq_len(nrow(b))) {
    if (a$sv[i] >= b$sv[j]) {
      sv <- a$sv[i]; sl <- a$sl[i]; so <- a$sl_open[i]
    } else {
      sv <- b$sv[j]; sl <- b$sl[j]; so <- b$sl_open[j]
    }
    if (a$ev[i] <= b$ev[j]) {
      ev <- a$ev[i]; el <- a$el[i]; eo <- a$el_open[i]
    } else {
      ev <- b$ev[j]; el <- b$el[j]; eo <- b$el_open[j]
    }
    if (sv <= ev)
      out[[length(out) + 1]] <- tibble(sv = sv, ev = ev, sl = sl, el = el,
                                       sl_open = so, el_open = eo)
  }
  if (length(out) == 0) return(empty_iv())
  bind_rows(out)
}

subtract_ranges <- function(a, b) {
  if (nrow(a) == 0) return(empty_iv())
  bm  <- merge_ranges(b)
  out <- list()
  for (i in seq_len(nrow(a))) {
    pieces <- list(a[i, ])
    for (j in seq_len(nrow(bm))) {
      new <- list()
      for (p in pieces) {
        if (bm$ev[j] < p$sv || bm$sv[j] > p$ev) { new <- c(new, list(p)); next }
        # left remainder: strictly BELOW the removed range
        if (bm$sv[j] - EPS > p$sv)
          new <- c(new, list(tibble(sv = p$sv, ev = bm$sv[j] - EPS,
                                    sl = p$sl, el = bm$sl[j],
                                    sl_open = p$sl_open, el_open = TRUE)))
        # right remainder: strictly ABOVE the removed range
        if (bm$ev[j] + EPS < p$ev)
          new <- c(new, list(tibble(sv = bm$ev[j] + EPS, ev = p$ev,
                                    sl = bm$el[j], el = p$el,
                                    sl_open = TRUE, el_open = p$el_open)))
      }
      pieces <- new
    }
    out <- c(out, pieces)
  }
  if (length(out) == 0) return(empty_iv())
  bind_rows(out) |> filter(sv <= ev)
}

# Split intervals at letter boundaries so `letter` can be used as a join key
# (a cross join of every code against every interval would be needlessly large).
split_by_letter <- function(df) {
  if (nrow(df) == 0) return(mutate(df, letter = character()))
  df |>
    mutate(.li = map2(sv, ev, ~ seq.int(floor(.x / STRIDE), floor(.y / STRIDE)))) |>
    unnest(cols = ".li") |>
    mutate(letter = LETTERS[.li + 1L],
           sv     = pmax(sv, .li * STRIDE),
           ev     = pmin(ev, (.li + 1) * STRIDE - EPS)) |>
    filter(sv <= ev) |>
    select(-".li")
}

# ---- per-cause comparison ----------------------------------------------------

compare_icd10_ranges <- function(non_fatal_str, cod_str) {
  tryCatch({
    nf  <- merge_ranges(parse_rangelist(non_fatal_str))
    cod <- merge_ranges(parse_rangelist(cod_str))
    tibble(overlap_iv              = list(intersect_ranges(nf, cod)),
           icd10_non_fatal_only_iv = list(subtract_ranges(nf, cod)),
           icd10_cod_only_iv       = list(subtract_ranges(cod, nf)),
           error                   = NA_character_)
  }, error = function(e) {
    tibble(overlap_iv              = list(empty_iv()),
           icd10_non_fatal_only_iv = list(empty_iv()),
           icd10_cod_only_iv       = list(empty_iv()),
           error                   = conditionMessage(e))
  })
}

# ==============================================================================
# 1. GBD 2019 cause list
# ==============================================================================

gbd_2019 <- read_xlsx(here("data/icd/lancet_conditions_icd10.xlsx")) |>
  select(cause                 = gbd_term,
         icd10_non_fatal       = gbd_non_fatal_icd_10,
         icd10_cod             = gbd_cod_icd_10,
         cause_hierarchy_level = gbd_level) |>
  distinct()

gbd_2019 <- gbd_2019 |>
  mutate(cmp = pmap(list(icd10_non_fatal, icd10_cod), compare_icd10_ranges)) |>
  unnest(cmp) |>
  mutate(overlap              = map_chr(overlap_iv,              show_ranges),
         icd10_non_fatal_only = map_chr(icd10_non_fatal_only_iv, show_ranges),
         icd10_cod_only       = map_chr(icd10_cod_only_iv,       show_ranges))

# Surface parse failures instead of silently dropping them.
parse_failures <- gbd_2019 |> filter(!is.na(error)) |> select(cause, error)
if (nrow(parse_failures) > 0){
  warning(nrow(parse_failures),
          " cause(s) had unparseable ICD-10 strings - see `parse_failures`.",
          call. = FALSE)
}

# ==============================================================================
# 2. One row per (cause, source, interval), keyed by leading letter
# ==============================================================================

cause_intervals <- gbd_2019 |>
  select(cause, cause_hierarchy_level, ends_with("_iv")) |>
  pivot_longer(ends_with("_iv"), names_to = "cause_source", values_to = "iv") |>
  mutate(cause_source = str_remove(cause_source, "_iv$")) |>
  filter(map_int(iv, nrow) > 0) |>
  unnest(iv) |>
  mutate(matched_range = fmt_iv(sl, el, sl_open, el_open)) |>
  split_by_letter() |>
  select(cause, cause_hierarchy_level, cause_source, letter, sv, ev, matched_range)

# ==============================================================================
# 3. Match the observed codes
# ==============================================================================


disease_mapping <- gwas_mesh |>
  mutate(icd10_code = case_when(is.na(icd10_code) & !is.na(icd10cm_code) ~ icd10cm_code,
                                TRUE~ icd10_code
  ))


code_intervals <- disease_mapping |>
  distinct(icd10_code) |>
  filter(!is.na(icd10_code), str_trim(icd10_code) != "") |>
  mutate(parsed = map(icd10_code, function(cd) {
    p <- try(parse_icd10_code(cd), silent = TRUE)
    if (inherits(p, "try-error"))
      return(tibble(letter = NA_character_, lo = NA_real_, hi = NA_real_,
                    parse_error = str_trim(as.character(p))))
    tibble(letter = p$letter, lo = p$lo, hi = p$hi, parse_error = NA_character_)
  })) |>
  unnest(parsed)

code_to_cause <- code_intervals |>
  left_join(cause_intervals, by = "letter", relationship = "many-to-many") |>
  # coalesce() is essential: unparseable codes give NA, and `any(NA)` would
  # otherwise blow up the `if` in the filter below.
  mutate(hit = coalesce(!is.na(sv) & lo <= ev & sv <= hi, FALSE)) |>
  group_by(icd10_code) |>
  filter(if (any(hit)) hit else row_number() == 1L) |>   # keep unmatched codes once
  ungroup() |>
  mutate(source_rank = match(cause_source, SRC)) |>
  arrange(icd10_code, source_rank, cause_hierarchy_level) |>
  mutate(across(c(cause, cause_hierarchy_level, cause_source, matched_range),
                ~ replace(.x, !hit, NA))) |>
  group_by(icd10_code) |>
  mutate(n_causes = sum(hit)) |>
  ungroup() |>
  select(icd10_code, cause, cause_hierarchy_level, cause_source,
         matched_range, n_causes, parse_error)

disease_mapping_annotated <- disease_mapping |>
  left_join(code_to_cause, by = "icd10_code", relationship = "many-to-many")

# ---- QA ----------------------------------------------------------------------

qa_codes <- code_to_cause |>
  summarise(n_codes      = n_distinct(icd10_code),
            n_unmatched  = n_distinct(icd10_code[is.na(cause)]),
            n_unparsed   = n_distinct(icd10_code[!is.na(parse_error)]),
            n_ambiguous  = n_distinct(icd10_code[n_causes > 1]))

code_to_cause

papers_disease <-
disease_mapping_annotated  |>
  distinct(pubmed_id, cause)  |>
  mutate(pubmed_id = as.integer(pubmed_id)) |>
  group_by(pubmed_id) |>
  summarise(cause = str_flatten(unique(cause),
                                 na.rm = T,
                                collapse = ";",
                                      )) |>
  mutate(cause = ifelse(cause == "", NA, cause))



manual_map_papers_disease <-
  data.table::fread(here::here("output/icd_map/gwas_study_gbd_causes.csv")) |>
  rename_with(~ tolower(str_replace_all(., " ", "_"))) |>
  distinct(pubmed_id, cause) |>
  mutate(pubmed_id = as.integer(pubmed_id)) |>
  group_by(pubmed_id) |>
  mutate(cause = ifelse(cause == "", NA, cause)) |>
  summarise(cause = str_flatten(unique(cause),
                                collapse = ";",
                                na.rm = T)) |>
  mutate(cause = ifelse(cause == "", NA, cause))


matched_causes <-
full_join(
  papers_disease |> rename(mesh_cause = cause),
  manual_map_papers_disease,
  by = c("pubmed_id"),
  relationship ="many-to-many"
)

# not matching pubmeds:
matched_causes |>
  filter((mesh_cause != cause) | (is.na(mesh_cause) & !is.na(cause)) | (!is.na(mesh_cause) & is.na(cause))) |>
  pull(pubmed_id) |>
  unique() |>
  length()

not_matching <- matched_causes |>
  filter((mesh_cause != cause) | (is.na(mesh_cause) & !is.na(cause)) | (!is.na(mesh_cause) & is.na(cause))) |>
  pull(pubmed_id) |>
  unique()

length(not_matching)

matched_causes |>
  filter((!pubmed_id %in% not_matching)) |>
  pull(pubmed_id) |>
  unique() |>
  length()

matched_causes  |>
  filter(is.na(mesh_cause) & !is.na(cause))

# 18991354

matched_causes |>
  filter((mesh_cause != cause) | (is.na(mesh_cause) & !is.na(cause)) | (!is.na(mesh_cause) & is.na(cause)))  |>
  mutate(mesh_cause = ifelse(is.na(mesh_cause), "none", "y"),
         cause = ifelse(is.na(cause), "none", "y"))|>
  group_by(mesh_cause, cause) |>
  summarise(n = n()) |>
  arrange(desc(n))


check_cause_paper_overlap <- function(cause_name){


manual_papers <-
  data.table::fread(here::here("output/icd_map/gwas_study_gbd_causes.csv")) |>
  rename_with(~ tolower(str_replace_all(., " ", "_"))) |>
  distinct(pubmed_id, cause) |>
  mutate(pubmed_id = as.integer(pubmed_id)) |>
  filter(cause == cause_name) |>
  pull(pubmed_id) |>
  unique()

papers_disease <-
  disease_mapping_annotated  |>
  distinct(pubmed_id, cause)  |>
  mutate(pubmed_id = as.integer(pubmed_id)) |>
  filter(cause == cause_name) |>
  pull(pubmed_id) |>
  unique()

print("Length of intersection")
print(length(intersect(manual_papers, papers_disease)))

print("Number by mesh")
print(length(setdiff(papers_disease, manual_papers)))

print("Number by pipeline")
print(length(setdiff(manual_papers, papers_disease)))

}


manual_papers <-
  data.table::fread(here::here("output/icd_map/gwas_study_gbd_causes.csv")) |>
  rename_with(~ tolower(str_replace_all(., " ", "_"))) |>
  distinct(pubmed_id, cause) |>
  mutate(pubmed_id = as.integer(pubmed_id)) |>
  filter(cause == "Cervical Cancer")
