#' Look up journal metadata from the OpenAlex "sources" API.
#'
#' GWAS Catalog stores journals as NLM abbreviations ("Circ Cardiovasc Genet"),
#' which do not always match an OpenAlex display_name. Two-step strategy:
#'   1. resolve the abbreviation to an ISSN via the NLM Catalog (E-utilities),
#'      then fetch the OpenAlex source by ISSN  (exact, preferred)
#'   2. if that fails, fall back to the OpenAlex ?search= parameter, which
#'      searches display_name, alternate_titles and abbreviated_title
#'
#' Usage:
#'   source(here::here("code/journal_meta/get_openalex_journal_info.R"))
#'   get_journal_info("Circ Cardiovasc Genet")
#'   openalex_search_sources("Circ Cardiovasc Genet")   # see all candidates
#'   journal_meta <- get_journals_info(unique(journal_names$JOURNAL))
#'
#' Set an email once per session so requests go to OpenAlex's faster
#' "polite pool" (their convention, no API key needed):
#'   Sys.setenv(OPENALEX_MAILTO = "you@example.com")

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

OPENALEX_BASE <- "https://api.openalex.org"
EUTILS_BASE   <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

# ---------------------------------------------------------------------------
# Low-level GET with polite-pool header, retries and rate limiting
# ---------------------------------------------------------------------------
api_get <- function(url, query = list(), max_tries = 4, pause = 0.15) {
  mailto <- Sys.getenv("OPENALEX_MAILTO", unset = "")
  if (grepl("openalex", url, fixed = TRUE)) {
    if (nzchar(mailto)) {
      query$mailto <- mailto
    } else {
      warning("Set Sys.setenv(OPENALEX_MAILTO = 'you@example.com') to use ",
              "OpenAlex's polite pool; unidentified requests are throttled.",
              call. = FALSE)
    }
  }

  for (try_i in seq_len(max_tries)) {
    Sys.sleep(pause)
    resp <- try(GET(url, query = query,
                    user_agent("journal-metadata-lookup (R/httr)"),
                    timeout(30)),
                silent = TRUE)

    if (!inherits(resp, "try-error")) {
      code <- status_code(resp)
      if (code == 200) {
        return(fromJSON(content(resp, as = "text", encoding = "UTF-8"),
                        simplifyVector = FALSE))
      }
      if (code == 404) return(NULL)                 # no such source
      if (!code %in% c(429, 500, 502, 503, 504)) {  # unrecoverable
        warning("HTTP ", code, " for ", url, call. = FALSE)
        return(NULL)
      }
    }
    Sys.sleep(2^try_i)  # back off on 429 / 5xx / network error
  }
  warning("Giving up on ", url, " after ", max_tries, " tries", call. = FALSE)
  NULL
}

# ---------------------------------------------------------------------------
# Step 1: NLM Catalog abbreviation -> ISSN + full title
# ---------------------------------------------------------------------------
nlm_lookup <- function(journal_abbrev) {
  empty <- list(issn = NA_character_, full_title = NA_character_)

  srch <- api_get(file.path(EUTILS_BASE, "esearch.fcgi"),
                  list(db = "nlmcatalog",
                       term = paste0('"', journal_abbrev, '"[Title Abbreviation]'),
                       retmode = "json", retmax = 1))
  ids <- srch$esearchresult$idlist
  if (is.null(ids) || length(ids) == 0) return(empty)

  summ <- api_get(file.path(EUTILS_BASE, "esummary.fcgi"),
                  list(db = "nlmcatalog", id = ids[[1]], retmode = "json"))
  rec <- summ$result[[ids[[1]]]]
  if (is.null(rec)) return(empty)

  # prefer the linking ISSN, else the first ISSN listed (print or electronic)
  issn <- rec$issnlinking
  if (is.null(issn) || !nzchar(issn)) {
    issn <- rec$issnlist |>
      map_chr(~ .x$issn %||% NA_character_) |>
      discard(is.na) |>
      first_or_na()
  }

  list(issn = issn %||% NA_character_,
       full_title = rec$titlemainlist[[1]]$title %||% rec$title %||% NA_character_)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
first_or_na <- function(x) if (length(x) == 0) NA_character_ else x[[1]]

# ---------------------------------------------------------------------------
# Step 2: OpenAlex source lookups
# ---------------------------------------------------------------------------
openalex_source_by_issn <- function(issn) {
  if (is.na(issn) || !nzchar(issn)) return(NULL)
  api_get(paste0(OPENALEX_BASE, "/sources/issn:", issn))
}

#' All candidate sources for a name, so you can eyeball an ambiguous match
openalex_search_sources <- function(journal, per_page = 10) {
  res <- api_get(paste0(OPENALEX_BASE, "/sources"),
                 list(search = journal, per_page = per_page))
  if (is.null(res) || length(res$results) == 0) return(tibble::tibble())
  map_dfr(res$results, source_to_row) |>
    mutate(query = journal, .before = 1)
}

# one OpenAlex source object -> one row
source_to_row <- function(src) {
  topics <- map_chr(src$topics %||% list(), ~ .x$display_name %||% NA_character_)
  fields <- map_chr(src$topics %||% list(),
                    ~ .x$field$display_name %||% NA_character_)

  tibble::tibble(
    openalex_id        = src$id %||% NA_character_,
    display_name       = src$display_name %||% NA_character_,
    abbreviated_title  = src$abbreviated_title %||% NA_character_,
    alternate_titles   = paste(unlist(src$alternate_titles %||% list()),
                               collapse = "; "),
    issn_l             = src$issn_l %||% NA_character_,
    issn               = paste(unlist(src$issn %||% list()), collapse = "; "),
    type               = src$type %||% NA_character_,
    publisher          = src$host_organization_name %||% NA_character_,
    society            = paste(map_chr(src$societies %||% list(),
                                       ~ .x$organization %||% NA_character_),
                               collapse = "; "),
    country_code       = src$country_code %||% NA_character_,
    is_oa              = src$is_oa %||% NA,
    is_in_doaj         = src$is_in_doaj %||% NA,
    is_core            = src$is_core %||% NA,
    apc_usd            = src$apc_usd %||% NA_integer_,
    works_count        = src$works_count %||% NA_integer_,
    cited_by_count     = src$cited_by_count %||% NA_integer_,
    h_index            = src$summary_stats$h_index %||% NA_integer_,
    two_yr_citedness   = src$summary_stats$`2yr_mean_citedness` %||% NA_real_,
    top_topics         = paste(head(topics, 3), collapse = "; "),
    top_fields         = paste(head(unique(discard(fields, is.na)), 2),
                               collapse = "; "),
    homepage_url       = src$homepage_url %||% NA_character_
  )
}

# ---------------------------------------------------------------------------
# Main entry point: one journal in, one row out
# ---------------------------------------------------------------------------
get_journal_info <- function(journal, use_nlm = TRUE) {
  na_row <- tibble::tibble(journal = journal,
                           match_method = "no match",
                           nlm_issn = NA_character_,
                           nlm_full_title = NA_character_)

  nlm <- if (use_nlm) nlm_lookup(journal) else list(issn = NA_character_,
                                                    full_title = NA_character_)

  src <- openalex_source_by_issn(nlm$issn)
  method <- "issn (via NLM Catalog)"

  # fall back to OpenAlex text search: full title first, then the abbreviation
  if (is.null(src)) {
    for (term in discard(c(nlm$full_title, journal), is.na)) {
      hit <- api_get(paste0(OPENALEX_BASE, "/sources"),
                     list(search = term, per_page = 1))
      if (!is.null(hit) && length(hit$results) > 0) {
        src <- hit$results[[1]]
        method <- paste0("search: ", term)
        break
      }
    }
  }

  if (is.null(src)) return(na_row)

  source_to_row(src) |>
    mutate(journal = journal,
           match_method = method,
           nlm_issn = nlm$issn,
           nlm_full_title = nlm$full_title,
           .before = 1)
}

# ---------------------------------------------------------------------------
# Batch version with an on-disk cache (safe to re-run / interrupt)
# ---------------------------------------------------------------------------
get_journals_info <- function(journals,
                              cache_path = here::here("output/journal_meta/openalex_journals.csv"),
                              use_nlm = TRUE) {
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

  cached <- if (file.exists(cache_path)) {
    data.table::fread(cache_path, colClasses = "character")
  } else {
    tibble::tibble(journal = character())
  }

  todo <- setdiff(unique(journals), cached$journal)
  message(length(todo), " journals to fetch (", nrow(cached), " cached)")

  if (length(todo) > 0) {
    fetched <- map_dfr(seq_along(todo), function(i) {
      if (i %% 25 == 0) message("  ", i, "/", length(todo))
      get_journal_info(todo[i], use_nlm = use_nlm)
    })
    out <- bind_rows(mutate(cached, across(everything(), as.character)),
                     mutate(fetched, across(everything(), as.character)))
    data.table::fwrite(out, cache_path)
  } else {
    out <- cached
  }

  out |>
    filter(journal %in% journals) |>
    type.convert(as.is = TRUE)
}
