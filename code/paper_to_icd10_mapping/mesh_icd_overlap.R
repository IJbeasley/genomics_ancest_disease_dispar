


################ 1. Measure overlap between GBD ICD Code Sources #############
B36 <- c(0:9, LETTERS)
v36 <- function(ch) match(toupper(ch), c(as.character(0:9), LETTERS)) - 1L

parse_icd10_code <- function(code) {
  raw <- str_trim(code)
  m <- str_match(raw, "^([A-Za-z])([0-9])([0-9A-Za-z])\\.?([0-9A-Za-z]*)$")
  if (is.na(m[1,1])) stop("Unparseable ICD10 code: '", raw, "'")
  sub <- m[1,5]
  frac <- 0
  if (nchar(sub) > 0) {
    chars <- str_split(sub, "")[[1]]
    frac <- sum(v36(chars) / 36^seq_along(chars))
  }
  list(letter = toupper(m[1,2]),
       value  = v36(m[1,3]) * 36 + v36(m[1,4]) + frac,
       label  = raw,
       bare   = nchar(sub) == 0)     # <-- new
}

parse_range <- function(rng) {
  parts <- str_trim(str_split(rng, "-")[[1]])
  s <- parse_icd10_code(parts[1])
  e <- parse_icd10_code(parts[length(parts)])
  if (s$letter != e$letter) stop("Range spans letters: '", rng, "'")
  # a bare 3-char END code covers its entire category
  ev <- if (e$bare) e$value + 1 - 1e-9 else e$value
  tibble(letter = s$letter, sv = s$value, ev = ev, sl = s$label, el = e$label)
}

parse_rangelist <- function(s) {
  if (is.na(s) || str_trim(s) == "")
    return(tibble(letter = character(), sv = numeric(), ev = numeric(),
                  sl = character(), el = character()))
  map_dfr(str_trim(str_split(s, ",")[[1]]), parse_range)
}

show_ranges <- function(df) {
  if (nrow(df) == 0) return(NA_character_)
  paste(if_else(df$sl == df$el, df$sl, paste0(df$sl, "-", df$el)), collapse = ", ")
}

intersect_ranges <- function(a, b) {
  out <- list()
  for (i in seq_len(nrow(a))) for (j in seq_len(nrow(b))) {
    if (a$letter[i] != b$letter[j]) next
    if (a$sv[i] >= b$sv[j]) { sv <- a$sv[i]; sl <- a$sl[i] } else { sv <- b$sv[j]; sl <- b$sl[j] }
    if (a$ev[i] <= b$ev[j]) { ev <- a$ev[i]; el <- a$el[i] } else { ev <- b$ev[j]; el <- b$el[j] }
    if (sv <= ev)
      out[[length(out)+1]] <- tibble(letter = a$letter[i], sv = sv, ev = ev, sl = sl, el = el)
  }
  bind_rows(out)
}

merge_ranges <- function(df) {
  if (nrow(df) == 0) return(df)
  out <- list()
  for (l in unique(df$letter)) {
    x <- df |> filter(letter == l) |> arrange(sv)
    cur <- x[1, ]
    for (k in seq_len(nrow(x))[-1]) {
      if (x$sv[k] <= cur$ev) {
        if (x$ev[k] > cur$ev) { cur$ev <- x$ev[k]; cur$el <- x$el[k] }
      } else { out[[length(out)+1]] <- cur; cur <- x[k, ] }
    }
    out[[length(out)+1]] <- cur
  }
  bind_rows(out)
}

subtract_ranges <- function(a, b) {
  bm <- merge_ranges(b)
  out <- list()
  for (i in seq_len(nrow(a))) {
    pieces <- list(a[i, ])
    bs <- bm |> filter(letter == a$letter[i]) |> arrange(sv)
    for (j in seq_len(nrow(bs))) {
      new <- list()
      for (p in pieces) {
        if (bs$ev[j] < p$sv || bs$sv[j] > p$ev) { new[[length(new)+1]] <- p; next }
        if (bs$sv[j] > p$sv)
          new[[length(new)+1]] <- tibble(letter = p$letter, sv = p$sv, ev = bs$sv[j],
                                         sl = p$sl, el = bs$sl[j])
        if (bs$ev[j] < p$ev)
          new[[length(new)+1]] <- tibble(letter = p$letter, sv = bs$ev[j], ev = p$ev,
                                         sl = bs$el[j], el = p$el)
      }
      pieces <- new
    }
    out <- c(out, pieces)
  }
  bind_rows(out)
}

compare_icd10_ranges <- function(cause, non_fatal_str, cod_str) {
  res <- try({
    nf <- parse_rangelist(non_fatal_str); cod <- parse_rangelist(cod_str)
    tibble(cause = cause,
           overlap              = show_ranges(intersect_ranges(nf, cod)),
           icd10_non_fatal_only = show_ranges(subtract_ranges(nf, cod)),
           icd10_cod_only       = show_ranges(subtract_ranges(cod, nf)),
           error = NA_character_)
  }, silent = TRUE)
  if (inherits(res, "try-error"))
    tibble(cause = cause, overlap = NA_character_, icd10_non_fatal_only = NA_character_,
           icd10_cod_only = NA_character_, error = as.character(res))
  else res
}


gbd_2019 <- readxl::read_xlsx(here::here("data/icd/lancet_conditions_icd10.xlsx"))

gbd_2019 =
  gbd_2019 |>
  select(cause = gbd_term,
         icd10_non_fatal = gbd_non_fatal_icd_10,
         icd10_cod = gbd_cod_icd_10,
         cause_hierarchy_level = gbd_level)

gbd_2019 <- gbd_2019 |>
  distinct(cause, icd10_non_fatal, icd10_cod) |>
  pmap_dfr(~ compare_icd10_ranges(..1, ..2, ..3))

gbd_2019 |> filter(!is.na(error))

gbd_2019 <-
  gbd_2019 |>
  select(-error)


################## 2.




SRC <- c("overlap",
         "icd10_non_fatal_only",
         "icd10_cod_only"
         )

# --- 1. explode gbd_2019$overlap into one row per interval, per cause ---
cause_intervals <- gbd_2019 |>
  filter(!is.na(overlap)) |>
  select(cause, overlap) |>
  mutate(iv = map(overlap, parse_rangelist)) |>
  unnest(iv) |>
  select(cause, overlap_range_letter = letter, sv, ev, sl, el) |>
  mutate(overlap_range = if_else(sl == el, sl, paste0(sl, "-", el)))

# sanity check: any reversed ranges?

cause_intervals <- gbd_2019 |>
  select(cause, all_of(SRC)) |>
  pivot_longer(all_of(SRC),
               names_to = "cause_source",
               values_to = "rangelist") |>
  filter(!is.na(rangelist)) |>
  mutate(iv = map(rangelist, ~ merge_ranges(parse_rangelist(.x)))) |>
  unnest(iv) |>
  filter(sv <= ev) |>                       # drop reversed ranges like K75.4-K75
  mutate(matched_range = if_else(sl == el, sl, paste0(sl, "-", el))) |>
  select(cause, cause_source, letter, sv, ev, matched_range)

code_intervals <- disease_mapping |>
  distinct(icd10_code) |>
  filter(!is.na(icd10_code), str_trim(icd10_code) != "") |>
  mutate(parsed = map(icd10_code, function(cd) {
    p <- try(parse_icd10_code(cd), silent = TRUE)
    if (inherits(p, "try-error"))
      return(tibble(letter = NA_character_, qsv = NA_real_, qev = NA_real_,
                    parse_error = as.character(p)))
    tibble(letter = p$letter, qsv = p$value,
           qev = if (p$bare) p$value + 1 - 1e-9 else p$value,
           parse_error = NA_character_)
  })) |>
  unnest(parsed)

code_to_cause <- code_intervals |>
  left_join(cause_intervals, by = "letter", relationship = "many-to-many") |>
  mutate(hit = !is.na(sv) & qsv <= ev & sv <= qev) |>
  group_by(icd10_code) |>
  filter(if (any(hit)) hit else row_number() == 1) |>
  ungroup() |>
  transmute(icd10_code,
            cause         = if_else(hit, cause, NA_character_),
            cause_source  = if_else(hit, cause_source, NA_character_),
            matched_range = if_else(hit, matched_range, NA_character_),
            parse_error)

disease_mapping_annotated <- disease_mapping |>
  left_join(code_to_cause, by = "icd10_code", relationship = "many-to-many")
