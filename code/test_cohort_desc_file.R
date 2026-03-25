
# checking for synonyms in the gwas cohort names
cohort_names <- readxl::read_xlsx(here::here("data/cohort/cohort_desc.xlsx"),
                                  sheet = 1) |>
  mutate(across(everything(),
                ~stringr::str_replace_all(.x,
                                          pattern = "\u00A0",
                                          replacement = " "))
  )

cohort_names |>
  dplyr::select(cohort, full_name, synonyms)

# Work with cohort, full_name, synonyms; keep row index for grouping
df <- cohort_names |>
  select(cohort, full_name, synonyms) |>
  mutate(row_id = row_number())

# ── 2. Union-Find helpers ─────────────────────────────────────────────────────

make_uf <- function(n) seq_len(n)          # initialise: each node is its own root

find <- function(parent, x) {
  while (parent[x] != x) {
    parent[x] <- parent[parent[x]]          # path compression
    x <- parent[x]
  }
  list(root = x, parent = parent)
}

union_nodes <- function(parent, x, y) {
  rx <- find(parent, x); parent <- rx$parent; rx <- rx$root
  ry <- find(parent, y); parent <- ry$parent; ry <- ry$root
  if (rx != ry) parent[rx] <- ry
  parent
}

# ── 3. Build lookup tables: normalised value → row indices ───────────────────

# Helper: build a named list mapping lower-case values to row indices
build_map <- function(values) {
  non_na <- which(!is.na(values))
  keys   <- tolower(str_trim(values[non_na]))
  split(non_na, keys)
}

cohort_map   <- build_map(df$cohort)
fullname_map <- build_map(df$full_name)
synonyms_map <- build_map(df$synonyms)


# ── 4. Run union-find across all three match types ───────────────────────────

n      <- nrow(df)
parent <- make_uf(n)

union_map <- function(parent, lookup_map) {
  for (idxs in lookup_map) {
    if (length(idxs) > 1) {
      for (j in seq(2, length(idxs))) {
        parent <- union_nodes(parent, idxs[1], idxs[j])
      }
    }
  }
  parent
}

# (a) rows sharing the same cohort ID
parent <- union_map(parent, cohort_map)

# (b) rows sharing the same full_name
parent <- union_map(parent, fullname_map)

# (c) cross-field matches: cohort ↔ full_name, cohort ↔ synonyms, full_name ↔ synonyms
cross_union <- function(parent, values, other_map) {
  for (i in seq_len(n)) {
    v <- values[i]
    if (!is.na(v)) {
      key <- tolower(str_trim(v))
      if (!is.null(other_map[[key]])) {
        for (j in other_map[[key]]) {
          parent <- union_nodes(parent, i, j)
        }
      }
    }
  }
  parent
}

parent <- cross_union(parent, df$cohort,    fullname_map)   # cohort  → full_name
parent <- cross_union(parent, df$cohort,    synonyms_map)   # cohort  → synonyms
parent <- cross_union(parent, df$full_name, cohort_map)     # full_name → cohort
parent <- cross_union(parent, df$synonyms,  cohort_map)     # synonyms  → cohort
parent <- cross_union(parent, df$full_name, synonyms_map)   # full_name → synonyms
parent <- cross_union(parent, df$synonyms,  fullname_map)   # synonyms  → full_name

# ── 5. Assign group IDs ───────────────────────────────────────────────────────

# Resolve every node to its root
roots <- vapply(seq_len(n), function(i) find(parent, i)$root, integer(1))

# Re-label roots as compact group IDs (1, 2, 3 …)
root_to_group <- match(roots, unique(roots))

df <- df |>
  mutate(group_id = root_to_group)

# ── 6. Summarise groups ───────────────────────────────────────────────────────

group_sizes <- df |>
  count(group_id, name = "group_size")

df <- df |>
  left_join(group_sizes, by = "group_id")

# Rows that belong to a multi-row group (i.e. have at least one match)
matched <- df |>
  filter(group_size > 1) |>
  arrange(group_size |> desc(), group_id, cohort)

# ── 7. Classify match type ────────────────────────────────────────────────────

# For each group, note which fields drove the link
classify_group <- function(grp) {
  cohorts   <- unique(na.omit(grp$cohort))
  fullnames <- unique(na.omit(grp$full_name))
  syns      <- unique(na.omit(grp$synonyms))

  has_cohort_dup   <- length(cohorts)   < nrow(grp) || any(tolower(cohorts) %in% tolower(fullnames)) ||
    any(tolower(cohorts) %in% tolower(syns))
  has_fullname_dup <- length(fullnames) < nrow(grp)
  has_syn_link     <- length(syns) > 0 && (
    any(tolower(syns) %in% tolower(cohorts)) |
      any(tolower(syns) %in% tolower(fullnames))
  )

  types <- c(
    if (has_cohort_dup)   "shared_cohort_id",
    if (has_fullname_dup) "shared_full_name",
    if (has_syn_link)     "synonym_link"
  )
  if (length(types) == 0) types <- "other"
  paste(types, collapse = "; ")
}

match_types <- matched |>
  group_by(group_id) |>
  group_modify(~tibble(match_type = classify_group(.x))) |>
  ungroup()

matched <- matched |>
  left_join(match_types, by = "group_id")

# ── 8. Output ─────────────────────────────────────────────────────────────────

cat(sprintf("Total rows: %d\n", n))
cat(sprintf("Groups with >1 row: %d\n", sum(group_sizes$group_size > 1)))
cat(sprintf("Rows involved in a match: %d\n\n", nrow(matched)))

# Print a readable summary of each multi-row group
matched |>
  arrange(desc(group_size), group_id) |>
  group_by(group_id) |>
  group_walk(function(grp, key) {
    cat(sprintf("─── Group %d  (size %d | %s) ───\n",
                key$group_id, grp$group_size[1], grp$match_type[1]))
    grp |>
      select(cohort, full_name, synonyms) |>
      print(n = Inf)
    cat("\n")
  })

# Return the full annotated data frame invisibly
invisible(matched)
