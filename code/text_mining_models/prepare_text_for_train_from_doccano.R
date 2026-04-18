


df <- stream_in(file(here::here("output/doccano/doccano_pt7_v5.jsonl")))


# incase multiple uploads of the same sentences to docanno, deduplicate by pubmed_id and text
df  <- df |>
  group_by(pubmed_id, text) |>
  slice_sample(n = 1) |>
  ungroup()

# df_long <- df |>
#   mutate(label = map(label, ~{
#     if (length(.x) == 0) return(NULL)
#     as.data.frame(.x)
#   })) |>
#   unnest(label)

df_long <- df |>
  mutate(label = map(label, ~{
    if (length(.x) == 0) return(NULL)
    as.data.frame(.x) |>
      setNames(c("start", "end", "label"))
  })) |>
  unnest(label, keep_empty = T) |>
  mutate(across(c(start, end), as.integer))

df_entities <-
df_long |> select(-Comments) |>
  mutate(entity_text = stringr::str_sub(text, start + 1, end)) |>
  mutate(entity_text = ifelse(is.na(entity_text), "", entity_text))

df_entities <-
  df_entities |>
  distinct()

df_entities |>
  filter(grepl(" $", entity_text))

# if white-space at the end of the entity, adjust the end position
# if white-space at the start of the entity, adjust the start position
df_entities <- df_entities |>
  mutate(
    end = if_else(grepl(" $", entity_text), end - 1L, end),
    start = if_else(grepl("^ ", entity_text), start + 1L, start)
  ) |>
  mutate(entity_text = stringr::str_sub(text, start + 1, end)) |>
  mutate(entity_text = ifelse(is.na(entity_text), "", entity_text))

df_entities |>
  group_by(text) |>
  summarise(has_entity = any(entity_text != "")) |>
  distinct() |>
  group_by(has_entity) |>
  summarise(n = n())


entity_texts <- df_entities |>
  filter(entity_text != "") |>
  pull(text) |>
  unique()

n_has_entities <- length(entity_texts)

df_entities |>
  filter(entity_text != "") |>
  select(pubmed_id, text) |>
  group_by(pubmed_id) |>
  summarise(n = n())


keep_non_entity_text <-
df_entities |>
  group_by(text) |>
  summarise(has_entity = any(entity_text != "")) |>
  filter(!has_entity) |>
  slice_sample(n = ceiling(0.2 * n_has_entities)) |>
  pull(text)

df_entities <-
  df_entities |>
  filter(text %in% keep_non_entity_text | entity_text != "")


# save the cleaned entities to a new jsonl file
build_label_list <- function(start, end, label) {
  keep <- !is.na(start) & !is.na(end) & !is.na(label)
  if (!any(keep)) return(list())
  pmap(list(start[keep], end[keep], label[keep]),
       \(s, e, l) list(s, e, l))
}

df_doc <- df_entities |>
  group_by(id, text, pubmed_id, date, country, gwas_cat_cohort_label) |>
  summarise(label = list(build_label_list(start, end, label)),
            .groups = "drop") |>
  mutate(
    # Doccano always has these as strings; cast defensively
    pubmed_id             = as.character(pubmed_id),
    date                  = as.character(date),
    country               = as.character(country),
    gwas_cat_cohort_label = as.character(gwas_cat_cohort_label),
    Comments              = list(list())
  )

out_path <- "output/doccano/abstracts_with_cohort_info_reconstructed.jsonl"
#dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

con <- file(out_path, "w", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

# con <- file(out_path, "w", encoding = "UTF-8")
# on.exit(close(con), add = TRUE)

for (i in seq_len(nrow(df_doc))) {
  tryCatch({
    obj <- list(
      #id                    = df_doc$id[[i]],
      text                  = df_doc$text[[i]],
      pubmed_id             = df_doc$pubmed_id[[i]],
      date                  = df_doc$date[[i]],
      country               = df_doc$country[[i]],
      gwas_cat_cohort_label = df_doc$gwas_cat_cohort_label[[i]],
      label                 = df_doc$label[[i]]
      #Comments              = df_doc$Comments[[i]]
    )

    json <- jsonlite::toJSON(obj,
                             auto_unbox = TRUE,
                             null       = "null",
                             na         = "null")

    # jsonlite serialises empty list() as {}; force [] for these two fields
    json <- gsub('"label":{}',    '"label":[]',    json, fixed = TRUE)
    json <- gsub('"Comments":{}', '"Comments":[]', json, fixed = TRUE)

    writeLines(json, con, useBytes = TRUE)
    flush(con)
  }, error = function(e) {
    message(sprintf("row %d (id=%s) failed: %s",
                    i, df_doc$id[[i]], conditionMessage(e)))
  })
}


# for (i in seq_len(nrow(df_doc))) {
#   obj <- list(
#     id                    = df_doc$id[[i]],
#     text                  = df_doc$text[[i]],
#     pubmed_id             = df_doc$pubmed_id[[i]],
#     date                  = df_doc$date[[i]],
#     country               = df_doc$country[[i]],
#     gwas_cat_cohort_label = df_doc$gwas_cat_cohort_label[[i]],
#     label                 = df_doc$label[[i]],
#     Comments              = df_doc$Comments[[i]]
#   )
#   json <- toJSON(obj, auto_unbox = TRUE, null = "null", na = "null")
#
#   # jsonlite writes empty list() as {} — force [] instead
#   json <- gsub('"label":{}',    '"label":[]',    json, fixed = TRUE)
#   json <- gsub('"Comments":{}', '"Comments":[]', json, fixed = TRUE)
#
#   writeLines(json, con)
# }
