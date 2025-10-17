

{



# # Function to fetch abstracts for multiple PMIDs
# get_pubmed_abstracts <- function(pmids) {
#   # Collapse PMIDs into a comma-separated string for Entrez
#   id_string <- paste(pmids, collapse = ",")
#
#   # Fetch XML from PubMed
#   xml_data <- entrez_fetch(db = "pubmed", id = id_string, rettype = "xml", parsed = FALSE)
#
#   # Parse XML
#   doc <- read_xml(xml_data)
#
#   # Find all articles
#   articles <- xml_find_all(doc, ".//PubmedArticle")
#
#   # Extract abstracts for each article
#   abstracts <- sapply(articles, function(article) {
#     abstract_nodes <- xml_find_all(article, ".//AbstractText")
#     if(length(abstract_nodes) > 0) {
#       paste(xml_text(abstract_nodes), collapse = " ")
#     } else {
#       "MISSING"  # If no abstract is present
#     }
#   })
#
#   return(abstracts)
# }



}

{







# Function to extract sentences containing "cohort names"
extract_cohort_sentences <- function(text_vector, cohort_names) {
  results <- lapply(seq_along(text_vector), function(i) {
    abstract <- text_vector[i]
    # For each cohort name, extract sentences
    do.call(rbind, lapply(cohort_names, function(cohort_name) {
      sentences <- unlist(str_split(abstract, "(?<=[.!?])\\s+"))
      matched <- sentences[str_detect(sentences, regex(cohort_name))]
      if(length(matched) > 0) {
        data.frame(
          abstract_id = i,
          sentence = matched,
          COHORT = cohort_name,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }))
  })
  # Combine all abstracts
  do.call(rbind, results)
}












# Convert JSON string back to data frame
# json_data <- fromJSON("abstracts_with_cohort_entities_simple.json",
#                       simplifyDataFrame = T,
#                       flatten = T)

#
# df <- json_data %>%
#   mutate(has_cohort = sapply(labels, length) > 0)
#
# # Split
# positive <- df_fixed %>% filter(has_cohort)
# negative <- df_fixed %>% filter(!has_cohort)
#
# # Sample negatives to match positives
# set.seed(123)
# negative_sample <- negative %>% sample_n(nrow(positive))
#
# # Combine
# balanced_df <- bind_rows(positive,
#                          negative_sample) %>% sample_frac(1)
#
# # Convert back to Doccano JSON
# {
# # balanced_list <-
# #   lapply(seq_len(nrow(balanced_df)), function(i) {
# #     list(
# #       text = balanced_df$text[i],
# #       label = unique(balanced_df$entities[[i]]),
# #       pubmed_id = balanced_df$pubmed_id[i],
# #       cohort = balanced_df$cohorts[i],
# #       date = balanced_df$date[i]
# #     )
# #   })
#   # Convert entities to list-of-lists format
#   balanced_df <- balanced_df %>%
#     rowwise() %>%
#     mutate(
#       labels = list(
#         lapply(entities, function(e) {
#           # If named list with start_offset/end_offset/label
#           if (length(e) > 0) {
#             c(e$start_offset, e$end_offset, e$label)
#           } else {
#             NULL
#           }
#         }) #%>% compact()  # remove NULLs
#       )
#     ) %>%
#     ungroup()
#
#   con <- file("balanced_abstracts.jsonl", "w")
#   for(i in seq_len(nrow(balanced_df))) {
#     json_line <- toJSON(
#       list(
#         text = balanced_df$text[i],
#         labels = balanced_df$labels[[i]],  # <- now list-of-lists
#         pubmed_id = balanced_df$pubmed_id[i],
#         cohort = balanced_df$cohorts[i],
#         date = balanced_df$date[i]
#       ),
#       auto_unbox = TRUE
#     )
#     writeLines(json_line, con)
#   }
#   close(con)
# }
#

